module User = Pool_user
module Id = Pool_common.Id

let get_or_failwith = Pool_common.Utils.get_or_failwith

type person =
  { uid : string
  ; email : string
  ; first_name : string
  ; last_name : string
  ; gender : string
  ; date_of_birth : string
  }
[@@deriving show, yojson] [@@yojson.allow_extra_fields]

type persons = person list [@@deriving show, yojson]

let answer_custom_fields fields contact =
  let open Custom_field in
  let open Public in
  let select_random options =
    Random.int (List.length options) |> CCList.nth options |> Answer.create
  in
  CCList.filter_map
    (fun field ->
      match (field : Public.t) with
      | Select (public, options, _) ->
        Public.Select (public, options, select_random options |> CCOption.pure)
        |> CCOption.pure
      | MultiSelect (public, options, _) ->
        Public.MultiSelect
          (public, options, select_random options |> CCList.pure)
        |> CCOption.pure
      | Boolean _ | Number _ | Text _ -> None)
    fields
  |> CCList.map (fun field ->
       Contact.PartialUpdate.Custom field
       |> fun update -> Contact.Updated (update, contact))
;;

let create_rand_persons n_persons =
  let open Cohttp in
  let open Cohttp_lwt_unix in
  let min_allowed = 1 in
  let max_allowed = 100 in
  if n_persons < min_allowed && n_persons > max_allowed
  then
    Logs.warn (fun m ->
      m
        "Contact generator: Limit number! (Allowed range from %d to %d)"
        min_allowed
        max_allowed);
  let api_url =
    Format.asprintf
      "https://random-data-api.com/api/v2/users?size=%d"
      (max min_allowed (min max_allowed n_persons))
  in
  let%lwt resp, body = Client.get (Uri.of_string api_url) in
  if resp |> Response.status |> Code.code_of_status |> CCInt.equal 200
  then (
    let%lwt json = Cohttp_lwt.Body.to_string body in
    json |> Yojson.Safe.from_string |> persons_of_yojson |> Lwt.return)
  else failwith "Could not find or decode any person data."
;;

let create_persons n_persons =
  let chunk_size = 100 in
  let sum = CCList.fold_left ( + ) 0 in
  let%lwt persons_chunked =
    n_persons
    |> CCFun.flip CCList.repeat [ 1 ]
    |> CCList.chunks chunk_size
    |> CCList.map sum
    |> Lwt_list.map_s create_rand_persons
  in
  CCList.flatten persons_chunked |> Lwt.return
;;

let admins db_pool =
  let open Utils.Lwt_result.Infix in
  let%lwt experimenter_roles =
    Experiment.find_all db_pool ()
    ||> CCList.map (fun { Experiment.id; _ } ->
          `Experimenter (Guard.Uuid.target_of Experiment.Id.value id))
  in
  let data =
    [ "The", "One", "admin@example.com", [ `Admin ] @ experimenter_roles
    ; "engineering", "admin", "engineering@econ.uzh.ch", [ `OperatorAll ]
    ; "Scooby", "Doo", "assistant@econ.uzh.ch", [ `LocationManagerAll ]
    ; "Winnie", "Pooh", "experimenter@econ.uzh.ch", [ `RecruiterAll ]
    ]
  in
  let ctx = Pool_tenant.to_ctx db_pool in
  let password =
    Sys.getenv_opt "POOL_ADMIN_DEFAULT_PASSWORD"
    |> CCOption.value ~default:"admin"
  in
  Lwt_list.iter_s
    (fun (given_name, name, email, (role : Guard.ActorRoleSet.elt list)) ->
      let%lwt user = Service.User.find_by_email_opt ~ctx email in
      match user with
      | None ->
        let%lwt admin =
          Service.User.create_admin ~ctx ~name ~given_name ~password email
        in
        let%lwt (_ : [> `Admin ] Guard.Authorizable.t) =
          admin
          |> Admin.create
          |> Admin.Guard.Actor.to_authorizable ~ctx
          |> Lwt.map Pool_common.Utils.get_or_failwith
        in
        let%lwt () =
          Guard.Persistence.Actor.grant_roles
            ~ctx
            (Guard.Uuid.Actor.of_string_exn admin.Sihl_user.id)
            Guard.ActorRoleSet.(CCList.fold_left (CCFun.flip add) empty role)
          |> Lwt.map CCResult.get_or_failwith
        in
        Lwt.return_unit
      | Some _ ->
        Logs.debug (fun m -> m "%s" "Admin user already exists");
        Lwt.return_unit)
    data
;;

let contacts db_pool =
  let n_contacts = 200 in
  let ctx = Pool_tenant.to_ctx db_pool in
  let combinations =
    let open CCList in
    let channels = Contact.RecruitmentChannel.all in
    let languages = Pool_common.Language.[ Some En; Some De; None ] in
    let terms_accepted_at = [ Some (Ptime_clock.now ()); None ] in
    let booleans = [ true; false ] in
    (fun a b c d e f -> a, b, c, d, e, f)
    <$> channels
    <*> languages
    <*> terms_accepted_at
    <*> booleans
    <*> booleans
    <*> booleans
  in
  let%lwt persons = create_persons n_contacts in
  let () =
    if n_contacts < CCList.length combinations
    then
      Logs.warn (fun m ->
        m
          "User seed: only %d out of %d possible combinations covered!"
          n_contacts
          (CCList.length combinations));
    ()
  in
  let users =
    persons
    |> CCList.mapi (fun idx person ->
         let channel, language, terms_accepted_at, paused, disabled, verified =
           CCList.get_at_idx_exn
             (idx mod CCList.length combinations)
             combinations
         in
         ( person.uid |> Id.of_string
         , person.first_name |> User.Firstname.of_string
         , person.last_name |> User.Lastname.of_string
         , person.email |> User.EmailAddress.of_string
         , language
         , channel
         , terms_accepted_at |> CCOption.map User.TermsAccepted.create
         , paused
         , disabled
         , verified ))
  in
  let password =
    Sys.getenv_opt "POOL_USER_DEFAULT_PASSWORD"
    |> CCOption.value ~default:"user"
    |> User.Password.create
    |> Pool_common.Utils.get_or_failwith
  in
  let%lwt () =
    let open Utils.Lwt_result.Infix in
    Lwt_list.fold_left_s
      (fun contacts
           ( user_id
           , firstname
           , lastname
           , email
           , language
           , recruitment_channel
           , terms_accepted_at
           , _
           , _
           , _ ) ->
        let%lwt user =
          Service.User.find_by_email_opt ~ctx (User.EmailAddress.value email)
        in
        Lwt.return
        @@
        match user with
        | None ->
          [ Contact.Created
              { Contact.user_id
              ; email
              ; password
              ; firstname
              ; lastname
              ; recruitment_channel = Some recruitment_channel
              ; terms_accepted_at
              ; language
              }
          ]
          @ contacts
        | Some { Sihl_user.id; _ } ->
          Logs.debug (fun m ->
            m
              "Contact already exists (%s): %s"
              (db_pool |> Pool_database.Label.value)
              id);
          contacts)
      []
      users
    >|> Lwt_list.iter_s (Contact.handle_event db_pool)
  in
  let open Utils.Lwt_result.Infix in
  Lwt_list.fold_left_s
    (fun contacts (user_id, _, _, _, _, _, _, paused, disabled, verified) ->
      let%lwt contact = Contact.find db_pool user_id in
      let%lwt custom_fields =
        let open Custom_field in
        find_all_by_contact db_pool user_id
        ||> fun (grouped, ungrouped) ->
        ungrouped
        @ CCList.flatten
            (CCList.map (fun { Group.Public.fields; _ } -> fields) grouped)
      in
      match contact with
      | Ok contact ->
        [ Contact.Updated
            ( Contact.PartialUpdate.Paused
                ( Pool_common.Version.create ()
                , paused |> Pool_user.Paused.create )
            , contact )
        ]
        @ (if disabled then [ Contact.Disabled contact ] else [])
        @ (if verified
          then
            Contact.EmailVerified contact
            :: answer_custom_fields custom_fields contact
          else [])
        @ contacts
        |> Lwt.return
      | Error err ->
        let _ = Pool_common.Utils.with_log_error ~level:Logs.Debug err in
        contacts |> Lwt.return)
    []
    users
  >|> Lwt_list.iter_s (Contact.handle_event db_pool)
;;
