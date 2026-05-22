open CCFun
open Ppx_yojson_conv_lib.Yojson_conv
module User = Pool_user
module Id = Pool_common.Id

let src = Logs.Src.create "database.seed.user"
let get_or_failwith = Pool_common.Utils.get_or_failwith

type person =
  { id : int
  ; uid : string
  ; email : string
  ; first_name : string
  ; last_name : string
  ; date_of_birth : string
  }
[@@deriving show, yojson] [@@yojson.allow_extra_fields]

type persons = person list [@@deriving show, yojson]

let answer_custom_fields fields contact =
  let open Custom_field in
  let open Public in
  let select_random options = Random.int (List.length options) |> CCList.nth options in
  let entity_uuid = Contact.(contact |> id |> Id.to_common) in
  CCList.filter_map
    (function
      | (Select (public, options, _) : Public.t) ->
        let answer = Some (select_random options) |> Answer.create entity_uuid in
        Public.Select (public, options, Some answer) |> CCOption.pure
      | MultiSelect (public, options, _) ->
        let answer = Some [ select_random options ] |> Answer.create entity_uuid in
        Public.MultiSelect (public, options, Some answer) |> CCOption.pure
      | Boolean _ | Date _ | Number _ | Text _ -> None)
    fields
  |> CCList.map (fun field ->
    let open Custom_field in
    PartialUpdate (PartialUpdate.Custom field, contact, Pool_context.Contact contact))
;;

let create_persons_from_file () =
  let sep = if Sys.win32 then CCString.of_char '\\' else "/" in
  Logs.info ~src (fun m -> m "Seed: load person data");
  CCString.concat sep [ Sys.getcwd (); "pool"; "seed"; "seed_user_data.json" ]
  |> Yojson.Safe.from_file
  |> persons_of_yojson
;;

let admins db_label =
  let open Utils.Lwt_result.Infix in
  let%lwt experimenter_roles =
    Experiment.all db_label
    ||> CCList.map (fun { Experiment.id; _ } ->
      `Experimenter, Some (Guard.Uuid.target_of Experiment.Id.value id))
  in
  let data =
    [ "The", "One", "admin@example.com", experimenter_roles
    ; "engineering", "admin", "it@econ.uzh.ch", [ `Operator, None ]
    ; "Scooby", "Doo", "assistant@econ.uzh.ch", [ `LocationManager, None ]
    ; "Winnie", "Pooh", "experimenter@econ.uzh.ch", [ `Recruiter, None ]
    ]
  in
  let ctx = Database.to_ctx db_label in
  let tags = Database.Logger.Tags.create db_label in
  let password =
    Sys.getenv_opt "POOL_ADMIN_DEFAULT_PASSWORD" |> CCOption.value ~default:"Password1!"
  in
  Lwt_list.iter_s
    (fun ( given_name
         , name
         , email
         , (roles : (Role.Role.t * Guard.Uuid.Target.t option) list) ) ->
       let email = Pool_user.EmailAddress.of_string email in
       let%lwt user = User.find_by_email_opt db_label email in
       match user with
       | None ->
         let%lwt admin =
           let id = Admin.Id.create () in
           let create =
             { Admin.id = id |> CCOption.return
             ; email
             ; password = Pool_user.Password.Plain.create password
             ; firstname = Pool_user.Firstname.of_string given_name
             ; lastname = Pool_user.Lastname.of_string name
             ; roles = []
             }
           in
           let%lwt () = Admin.Created create |> Admin.handle_event ~tags db_label in
           Admin.find db_label id |> Lwt.map CCResult.get_exn
         in
         let%lwt (_ : Guard.Target.t) =
           admin |> Admin.Guard.Target.to_authorizable ~ctx ||> get_or_failwith
         in
         let%lwt () =
           let open Guard in
           roles
           |> Lwt_list.iter_s (fun (role, target_uuid) ->
             ActorRole.create
               ?target_uuid
               (Uuid.Actor.of_string_exn Admin.(id admin |> Id.value))
               role
             |> Persistence.ActorRole.upsert ~ctx)
           ||> tap (fun _ -> Persistence.Cache.clear ())
         in
         Lwt.return_unit
       | Some _ ->
         Logs.debug ~src (fun m ->
           m ~tags:(Database.Logger.Tags.create db_label) "%s" "Admin user already exists");
         Lwt.return_unit)
    data
;;

let contacts db_label =
  let open Utils.Lwt_result.Infix in
  let tags = Database.Logger.Tags.create db_label in
  let n_contacts = 200 in
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
  let persons = create_persons_from_file () in
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
        CCList.get_at_idx_exn (idx mod CCList.length combinations) combinations
      in
      ( person.uid |> Contact.Id.of_string
      , person.first_name |> User.Firstname.of_string
      , person.last_name |> User.Lastname.of_string
      , CCFormat.asprintf "test+%i@econ.uzh.ch" idx |> User.EmailAddress.of_string
      , language
      , terms_accepted_at |> CCOption.map User.TermsAccepted.create
      , paused
      , disabled
      , verified ))
  in
  let password =
    Sys.getenv_opt "POOL_USER_DEFAULT_PASSWORD"
    |> CCOption.value ~default:"Password1!"
    |> User.Password.Plain.create
  in
  let%lwt () =
    users
    |> Lwt_list.filter_map_s
         (fun
             (user_id, firstname, lastname, email, language, terms_accepted_at, _, _, _)
            ->
            match%lwt Pool_user.find_by_email_opt db_label email with
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
            | Some { Pool_user.id; _ } ->
              Logs.debug ~src (fun m ->
                m
                  ~tags
                  "Contact already exists (%s): %a"
                  (db_label |> Database.Label.value)
                  Pool_user.Id.pp
                  id);
              Lwt.return_none)
    ||> CCList.flatten
    >|> Lwt_list.iter_s (Contact.handle_event db_label)
  in
  Logs.info ~src (fun m -> m ~tags "Seed: add additional infos to contacts");
  let%lwt contact_events, field_events =
    Lwt_list.fold_left_s
      (fun (contacts, fields) (user_id, _, _, _, _, _, paused, disabled, verified) ->
         let%lwt contact = Contact.find db_label user_id in
         let custom_fields contact =
           let open Custom_field in
           find_all_by_contact db_label (Pool_context.Contact contact) user_id
           ||> fun (grouped, ungrouped) ->
           ungrouped @ CCList.flat_map (fun { Group.Public.fields; _ } -> fields) grouped
         in
         match contact with
         | Ok contact ->
           let%lwt custom_fields = custom_fields contact in
           let field_events =
             if verified then answer_custom_fields custom_fields contact else []
           in
           let contact_events =
             if disabled
             then
               [ Contact.(
                   Updated
                     { contact with
                       paused = Pool_user.Paused.create paused
                     ; disabled = Pool_user.Disabled.create true
                     })
               ]
             else [] @ if verified then [ Contact.EmailVerified contact ] else []
           in
           (contacts @ contact_events, fields @ field_events) |> Lwt.return
         | Error err ->
           let _ = Pool_common.Utils.with_log_error ~src ~tags ~level:Logs.Debug err in
           (contacts, fields) |> Lwt.return)
      ([], [])
      users
  in
  let%lwt () = contact_events |> Lwt_list.iter_s (Contact.handle_event db_label) in
  field_events |> Lwt_list.iter_s (Custom_field.handle_event db_label)
;;
