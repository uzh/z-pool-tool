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
  let code = resp |> Response.status |> Code.code_of_status in
  if code = 200
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
  let data =
    [ "The", "One", "admin@example.com", `Operator
    ; "engineering", "admin", "engineering@econ.uzh.ch", `Operator
    ; "Scooby", "Doo", "assistant@econ.uzh.ch", `Assistant
    ; "Winnie", "Pooh", "experimenter@econ.uzh.ch", `Experimenter
    ]
  in
  let ctx = Pool_tenant.to_ctx db_pool in
  let password =
    Sys.getenv_opt "POOL_ADMIN_DEFAULT_PASSWORD"
    |> CCOption.value ~default:"admin"
  in
  Lwt_list.iter_s
    (fun (given_name, name, email, role) ->
      let%lwt user = Service.User.find_by_email_opt ~ctx email in
      match user with
      | None ->
        let%lwt user =
          Service.User.create_admin ~ctx ~name ~given_name ~password email
        in
        let person = Admin.create_person user in
        let%lwt () =
          match role with
          | `Assistant -> Admin.insert db_pool (Admin.Assistant person)
          | `Experimenter -> Admin.insert db_pool (Admin.Experimenter person)
          | `Recruiter -> Admin.insert db_pool (Admin.Recruiter person)
          | `LocationManager ->
            Admin.insert db_pool (Admin.LocationManager person)
          | `Operator -> Admin.insert db_pool (Admin.Operator person)
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
    let open Lwt.Infix in
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
              ; recruitment_channel
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
    >>= Lwt_list.iter_s (Contact.handle_event db_pool)
  in
  let open Lwt.Infix in
  Lwt_list.fold_left_s
    (fun contacts (user_id, _, _, _, _, _, _, paused, disabled, verified) ->
      let%lwt contact = Contact.find db_pool user_id in
      match contact with
      | Ok contact ->
        [ Contact.Updated
            ( Contact.PartialUpdate.Paused
                ( Pool_common.Version.create ()
                , paused |> Pool_user.Paused.create )
            , contact )
        ]
        @ (if disabled then [ Contact.Disabled contact ] else [])
        @ (if verified then [ Contact.EmailVerified contact ] else [])
        @ contacts
        |> Lwt.return
      | Error err ->
        let _ = Pool_common.Utils.with_log_error ~level:Logs.Debug err in
        contacts |> Lwt.return)
    []
    users
  >>= Lwt_list.iter_s (Contact.handle_event db_pool)
;;
