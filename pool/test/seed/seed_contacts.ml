module User = Pool_user

let range = CCList.range 0 10

let data =
  let create_contact i =
    let open Pool_common in
    let open Pool_user in
    let get = CCResult.get_exn in
    let id = Id.create () in
    let first_name =
      i |> Format.asprintf "firstname%i" |> Firstname.create |> get
    in
    let last_name =
      i |> Format.asprintf "lastname%i" |> Lastname.create |> get
    in
    let email =
      "contact-" ^ CCInt.to_string i ^ "@econ.uzh.ch"
      |> EmailAddress.create
      |> get
    in
    let lang = Pool_common.Language.En |> CCOption.pure in
    let recruitment_channel = Contact.RecruitmentChannel.Friend in
    let terms_accepted_at = TermsAccepted.create_now () |> CCOption.pure in
    ( id
    , first_name
    , last_name
    , email
    , lang
    , recruitment_channel
    , terms_accepted_at )
  in
  range |> CCList.map create_contact
;;

let contact_ids = CCList.map (fun (id, _, _, _, _, _, _) -> id) data

let create db_pool =
  let open Lwt.Infix in
  let ctx = Pool_tenant.to_ctx db_pool in
  (* let error = CCList.map (fun (k, v) -> Format.asprintf "%s: %s" k v) ctx |>
     CCString.concat " +++ " in failwith error; *)
  let password =
    Sys.getenv_opt "POOL_USER_DEFAULT_PASSWORD"
    |> CCOption.value ~default:"user"
    |> User.Password.create
    |> Pool_common.Utils.get_or_failwith
  in
  let%lwt () =
    Lwt_list.fold_left_s
      (fun contacts
           ( user_id
           , firstname
           , lastname
           , email
           , language
           , recruitment_channel
           , terms_accepted_at ) ->
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
      data
    >>= Lwt_list.iter_s (Contact.handle_event db_pool)
  in
  Lwt_list.map_s
    (fun (id, _, _, _, _, _, _) ->
      let%lwt contact = Contact.find db_pool id in
      match contact with
      | Ok contact ->
        [ Contact.EmailVerified contact
        ; Contact.TermsAccepted contact
        ; Contact.Verified contact
        ]
        |> CCOption.pure
        |> Lwt.return
      | Error _ -> Lwt.return_none)
    data
  |> Lwt.map (CCList.filter_map CCFun.id)
  >|= CCList.flatten
  >>= Lwt_list.iter_s (Contact.handle_event db_pool)
;;

let cleanup db_pool =
  let delete_sihl_user_request =
    let open Caqti_request.Infix in
    {sql|
      DELETE FROM user_users
      WHERE uuid = UNHEX(REPLACE(?, '-', ''))
    |sql}
    |> Caqti_type.(string ->. unit)
  in
  let delete_sihl_user id =
    Utils.Database.exec
      (Pool_database.Label.value db_pool)
      delete_sihl_user_request
      (Pool_common.Id.value id)
  in
  Lwt_list.iter_s delete_sihl_user contact_ids
;;
