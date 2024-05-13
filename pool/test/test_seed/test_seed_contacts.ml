module User = Pool_user

let src = Logs.Src.create "test.database.seed.contacts"
let default_range = CCList.range 0 10

let create_contact i =
  let open Pool_user in
  let id = Contact.Id.create () in
  let first_name = Format.asprintf "firstname%i" i |> Firstname.of_string in
  let last_name = Format.asprintf "lastname%i" i |> Lastname.of_string in
  let email =
    Format.asprintf "contact-%i@econ.uzh.ch" i |> EmailAddress.of_string
  in
  let lang = Pool_common.Language.En in
  let terms_accepted_at = TermsAccepted.create_now () in
  id, first_name, last_name, email, Some lang, Some terms_accepted_at
;;

let data = default_range |> CCList.map create_contact
let contact_ids = CCList.map (fun (id, _, _, _, _, _) -> id) data

let create ?(contact_data = data) db_pool =
  let open Contact in
  let open Utils.Lwt_result.Infix in
  let password =
    Sys.getenv_opt "POOL_USER_DEFAULT_PASSWORD"
    |> CCOption.value ~default:"Password1!"
    |> User.Password.Plain.create
  in
  let%lwt contact_data =
    Lwt_list.filter_s
      (fun (_, _, _, email, _, _) ->
        Pool_user.find_by_email_opt db_pool email ||> CCOption.is_none)
      contact_data
  in
  let%lwt () =
    Lwt_list.iter_s
      (fun (user_id, firstname, lastname, email, language, terms_accepted_at) ->
        let%lwt () =
          Contact.Created
            { Contact.user_id
            ; email
            ; password
            ; firstname
            ; lastname
            ; terms_accepted_at
            ; language
            }
          |> handle_event db_pool
        in
        match%lwt find db_pool user_id with
        | Ok contact ->
          let%lwt user = Pool_user.confirm db_pool contact.user in
          let contact =
            { contact with
              user
            ; email_verified = Some (Pool_user.EmailVerified.create_now ())
            ; terms_accepted_at = Some (User.TermsAccepted.create_now ())
            ; verified = Some (Pool_user.Verified.create_now ())
            }
          in
          Updated contact |> Contact.handle_event db_pool
        | Error _ -> failwith "Seeded test contact not found!")
      contact_data
  in
  Lwt.return_unit
;;

let find_contact_by_id pool id =
  id
  |> Format.asprintf "contact-%i@econ.uzh.ch"
  |> Pool_user.EmailAddress.of_string
  |> Contact.find_by_email pool
  |> Lwt.map CCResult.get_exn
;;
