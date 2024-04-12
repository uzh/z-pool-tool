module User = Pool_user

let src = Logs.Src.create "test.database.seed.contacts"
let defafult_range = CCList.range 0 10

let create_contact i =
  let open Pool_common in
  let open Pool_user in
  let get = CCResult.get_exn in
  let id = Id.create () in
  let first_name =
    i |> Format.asprintf "firstname%i" |> Firstname.create |> get
  in
  let last_name = i |> Format.asprintf "lastname%i" |> Lastname.create |> get in
  let email =
    "contact-" ^ CCInt.to_string i ^ "@econ.uzh.ch"
    |> EmailAddress.create
    |> get
  in
  let lang = Pool_common.Language.En |> CCOption.pure in
  let terms_accepted_at = TermsAccepted.create_now () |> CCOption.pure in
  id, first_name, last_name, email, lang, terms_accepted_at
;;

let data = defafult_range |> CCList.map create_contact
let contact_ids = CCList.map (fun (id, _, _, _, _, _) -> id) data

let create ?contact_data db_pool =
  let open Contact in
  let data = CCOption.value ~default:data contact_data in
  let open Utils.Lwt_result.Infix in
  let password =
    Sys.getenv_opt "POOL_USER_DEFAULT_PASSWORD"
    |> CCOption.value ~default:"Password1!"
    |> User.Password.create
    |> Pool_common.Utils.get_or_failwith
  in
  let%lwt () =
    Lwt_list.fold_left_s
      (fun contacts
        (user_id, firstname, lastname, email, language, terms_accepted_at) ->
        let%lwt user =
          Pool_user.find_by_email_opt db_pool (User.EmailAddress.value email)
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
              ; terms_accepted_at
              ; language
              }
          ]
          @ contacts
        | Some { Sihl_user.id; _ } ->
          Logs.debug ~src (fun m ->
            m
              ~tags:(Database.Logger.Tags.create db_pool)
              "Contact already exists (%s): %s"
              (db_pool |> Database.Label.value)
              id);
          contacts)
      []
      data
    >|> Lwt_list.iter_s (handle_event db_pool)
  in
  Lwt_list.map_s
    (fun (id, _, _, _, _, _) ->
      let%lwt contact = find db_pool id in
      match contact with
      | Ok contact ->
        let%lwt _ =
          Pool_user.update
            db_pool
            Sihl_user.{ contact.user with confirmed = true }
        in
        let contact =
          { contact with
            email_verified = Some (Pool_user.EmailVerified.create_now ())
          ; terms_accepted_at = Some (User.TermsAccepted.create_now ())
          ; verified = Some (Pool_user.Verified.create_now ())
          }
        in
        [ Updated contact ] |> CCOption.pure |> Lwt.return
      | Error _ -> Lwt.return_none)
    data
  ||> CCList.filter_map CCFun.id
  ||> CCList.flatten
  >|> Lwt_list.iter_s (Contact.handle_event db_pool)
;;

let find_contact_by_id pool id =
  id
  |> Format.asprintf "contact-%i@econ.uzh.ch"
  |> Pool_user.EmailAddress.of_string
  |> Contact.find_by_email pool
  |> Lwt.map CCResult.get_exn
;;
