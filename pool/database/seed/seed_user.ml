open CCFun
module User = Pool_user
module Id = Pool_common.Id

let src = Logs.Src.create "database.seed.user"
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
    Random.int (List.length options) |> CCList.nth options
  in
  CCList.filter_map
    (fun field ->
      match (field : Public.t) with
      | Select (public, options, _) ->
        Public.Select
          ( public
          , options
          , select_random options
            |> CCOption.pure
            |> Answer.create
            |> CCOption.pure )
        |> CCOption.pure
      | MultiSelect (public, options, _) ->
        Public.MultiSelect
          ( public
          , options
          , select_random options
            |> CCList.pure
            |> CCOption.pure
            |> Answer.create
            |> CCOption.pure )
        |> CCOption.pure
      | Boolean _ | Number _ | Text _ -> None)
    fields
  |> CCList.map (fun field ->
       let open Custom_field in
       PartialUpdate
         (PartialUpdate.Custom field, contact, Pool_context.Contact contact))
;;

let create_rand_persons ?tags n_persons =
  let open Cohttp in
  let open Cohttp_lwt_unix in
  let min_allowed = 1 in
  let max_allowed = 100 in
  if n_persons < min_allowed && n_persons > max_allowed
  then
    Logs.warn ~src (fun m ->
      m
        ?tags
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
  let open CCList in
  let open Utils.Lwt_result.Infix in
  let tags = Pool_database.Logger.Tags.create db_label in
  let chunk_size = 100 in
  let sum = fold_left ( + ) 0 in
  let%lwt contacts =
    Contact.find_all db_label ()
    ||> fst %> map (Contact.email_address %> User.EmailAddress.value)
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
    %> Lwt_list.map_s (create_rand_persons ~tags)
  in
  let rec persons_chunked acc =
    let current_count = length acc in
    let log_amount current =
      Logs.info ~src (fun m ->
        m ~tags "Seed: generating person data (%i/%i)" current n_persons)
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
    ||> fst
        %> CCList.map (fun { Experiment.id; _ } ->
             `Experimenter (Guard.Uuid.target_of Experiment.Id.value id))
  in
  let data =
    [ "The", "One", "admin@example.com", [ `Admin ] @ experimenter_roles
    ; ( "engineering"
      , "admin"
      , "engineering@econ.uzh.ch"
      , [ `OperatorAll
        ; `RecruiterAll
        ; `ManageAssistants
        ; `ManageExperimenters
        ; `ManageLocationManagers
        ; `ManageRecruiters
        ] )
    ; "Scooby", "Doo", "assistant@econ.uzh.ch", [ `LocationManagerAll ]
    ; ( "Winnie"
      , "Pooh"
      , "experimenter@econ.uzh.ch"
      , [ `RecruiterAll; `ManageAssistants; `ManageExperimenters ] )
    ]
  in
  let ctx = Pool_database.to_ctx db_label in
  let password =
    Sys.getenv_opt "POOL_ADMIN_DEFAULT_PASSWORD"
    |> CCOption.value ~default:"admin"
  in
  Lwt_list.iter_s
    (fun (given_name, name, email, (role : Guard.RoleSet.elt list)) ->
      let%lwt user = Service.User.find_by_email_opt ~ctx email in
      match user with
      | None ->
        let%lwt admin =
          Service.User.create_admin ~ctx ~name ~given_name ~password email
        in
        let%lwt (_ : Role.Actor.t Guard.Actor.t) =
          let admin = admin |> Admin.create in
          let%lwt (_ : Role.Target.t Guard.Target.t) =
            admin |> Admin.Guard.Target.to_authorizable ~ctx ||> get_or_failwith
          in
          admin |> Admin.Guard.Actor.to_authorizable ~ctx ||> get_or_failwith
        in
        let%lwt () =
          let open Guard in
          Persistence.Actor.grant_roles
            ~ctx
            (Uuid.Actor.of_string_exn admin.Sihl_user.id)
            RoleSet.(CCList.fold_left (flip add) empty role)
          ||> CCResult.get_or_failwith
          ||> tap (fun _ -> Persistence.Cache.clear ())
        in
        Lwt.return_unit
      | Some _ ->
        Logs.debug ~src (fun m ->
          m
            ~tags:(Pool_database.Logger.Tags.create db_label)
            "%s"
            "Admin user already exists");
        Lwt.return_unit)
    data
;;

let contacts db_label =
  let open Utils.Lwt_result.Infix in
  let tags = Pool_database.Logger.Tags.create db_label in
  let n_contacts = 200 in
  let ctx = Pool_database.to_ctx db_label in
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
  Logs.info ~src (fun m -> m ~tags "Seed: start generate contacts");
  let%lwt persons = create_persons db_label n_contacts in
  let () =
    if n_contacts < CCList.length combinations
    then
      Logs.warn ~src (fun m ->
        m
          ~tags
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
    |> CCOption.value ~default:"Password1!"
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
           Logs.debug ~src (fun m ->
             m
               ~tags
               "Contact already exists (%s): %s"
               (db_label |> Pool_database.Label.value)
               id);
           Lwt.return_none)
    ||> CCList.flatten
    >|> Lwt_list.iter_s (Contact.handle_event db_label)
  in
  Logs.info ~src (fun m -> m ~tags "Seed: add additional infos to contacts");
  let%lwt contact_events, field_events =
    Lwt_list.fold_left_s
      (fun (contacts, fields)
           (user_id, _, _, _, _, _, paused, disabled, verified) ->
        let%lwt contact = Contact.find db_label user_id in
        let custom_fields contact =
          let open Custom_field in
          find_all_by_contact db_label (Pool_context.Contact contact) user_id
          ||> fun (grouped, ungrouped) ->
          ungrouped
          @ CCList.flat_map (fun { Group.Public.fields; _ } -> fields) grouped
        in
        match contact with
        | Ok contact ->
          let%lwt custom_fields = custom_fields contact in
          let field_events =
            [ Custom_field.PartialUpdate
                ( Custom_field.PartialUpdate.Paused
                    ( Pool_common.Version.create ()
                    , paused |> Pool_user.Paused.create )
                , contact
                , Pool_context.Contact contact )
            ]
            @
            if verified then answer_custom_fields custom_fields contact else []
          in
          let contact_events =
            if disabled
            then [ Contact.Disabled contact ]
            else [] @ if verified then [ Contact.EmailVerified contact ] else []
          in
          (contacts @ contact_events, fields @ field_events) |> Lwt.return
        | Error err ->
          let _ =
            Pool_common.Utils.with_log_error ~tags ~level:Logs.Debug err
          in
          (contacts, fields) |> Lwt.return)
      ([], [])
      users
  in
  let%lwt () =
    contact_events |> Lwt_list.iter_s (Contact.handle_event db_label)
  in
  field_events |> Lwt_list.iter_s (Custom_field.handle_event db_label)
;;
