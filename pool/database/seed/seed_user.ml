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
    (* NOTE: API returns object directly if only 1 is requested *)
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

let create_persons db_label n_persons =
  let open CCFun in
  let open CCList in
  let open Utils.Lwt_result.Infix in
  let chunk_size = 100 in
  let sum = fold_left ( + ) 0 in
  let%lwt contacts =
    Contact.find_all db_label ()
    ||> map (Contact.email_address %> User.EmailAddress.value)
  in
  let%lwt admins = Admin.find_all db_label () ||> map Admin.email in
  let flatten_filter_combine a b =
    let filter_existing =
      filter (fun { email; _ } -> mem email (admins @ contacts) |> not)
    in
    (flatten b |> filter_existing) @ a
    |> uniq ~eq:(fun p1 p2 -> CCString.equal p1.email p2.email)
  in
  let generate_persons =
    flip repeat [ 1 ]
    %> chunks chunk_size
    %> map sum
    %> Lwt_list.map_s create_rand_persons
  in
  let rec persons_chunked acc =
    let current_count = length acc in
    let log_amount current =
      Logs.info (fun m ->
        m "Seed: generating person data (%i/%i)" current n_persons)
    in
    if current_count < n_persons
    then (
      log_amount current_count;
      let%lwt iter =
        max 2 (n_persons - current_count)
        |> generate_persons
        ||> flatten_filter_combine acc
      in
      persons_chunked iter)
    else (
      log_amount n_persons;
      take_drop n_persons acc |> fst |> Lwt.return)
  in
  persons_chunked []
;;

let admins db_label =
  let open Utils.Lwt_result.Infix in
  let%lwt experimenter_roles =
    Experiment.find_all db_label ()
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
  let ctx = Pool_tenant.to_ctx db_label in
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

let contacts db_label =
  let open Utils.Lwt_result.Infix in
  let n_contacts = 200 in
  let ctx = Pool_tenant.to_ctx db_label in
  let combinations =
    let open CCList in
    let languages = Pool_common.Language.[ Some En; Some De; None ] in
    let terms_accepted_at = [ Some (Ptime_clock.now ()); None ] in
    let booleans = [ true; false ] in
    (fun a b c d e -> a, b, c, d, e)
    <$> languages
    <*> terms_accepted_at
    <*> booleans
    <*> booleans
    <*> booleans
  in
  Logs.info (fun m -> m "Seed: start generate contacts");
  let%lwt persons = create_persons db_label n_contacts in
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
         let language, terms_accepted_at, paused, disabled, verified =
           CCList.get_at_idx_exn
             (idx mod CCList.length combinations)
             combinations
         in
         ( person.uid |> Id.of_string
         , person.first_name |> User.Firstname.of_string
         , person.last_name |> User.Lastname.of_string
         , person.email |> User.EmailAddress.of_string
         , language
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
    users
    |> Lwt_list.filter_map_s
         (fun
           ( user_id
           , firstname
           , lastname
           , email
           , language
           , terms_accepted_at
           , _
           , _
           , _ )
         ->
         match%lwt
           Service.User.find_by_email_opt ~ctx (User.EmailAddress.value email)
         with
         | None ->
           Lwt.return_some
             [ Contact.Created
                 { Contact.user_id
                 ; email
                 ; password
                 ; firstname
                 ; lastname
                 ; terms_accepted_at
                 ; language
                 }
             ]
         | Some { Sihl_user.id; _ } ->
           Logs.debug (fun m ->
             m
               "Contact already exists (%s): %s"
               (db_label |> Pool_database.Label.value)
               id);
           Lwt.return_none)
    ||> CCList.flatten
    >|> Lwt_list.iter_s (Contact.handle_event db_label)
  in
  Logs.info (fun m -> m "Seed: add additional infos to contacts");
  Lwt_list.fold_left_s
    (fun contacts (user_id, _, _, _, _, _, paused, disabled, verified) ->
      let%lwt contact = Contact.find db_label user_id in
      let%lwt custom_fields =
        let open Custom_field in
        find_all_by_contact db_label user_id
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
  >|> Lwt_list.iter_s (Contact.handle_event db_label)
;;
