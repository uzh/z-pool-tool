let context ?(is_root = false) () =
  let open Utils.Lwt_result.Infix in
  let open Pool_context in
  let find_query_language = Http_utils.find_query_lang in
  let tenant_database_label_of_request req =
    match Tenant.find req with
    | Ok { Tenant.tenant; _ } -> Lwt.return_ok tenant.Pool_tenant.database_label
    | Error _ ->
      Middleware_tenant.tenant_of_request req
      >|+ fun { Pool_tenant.database_label; _ } -> database_label
  in
  let database_label_of_request req =
    if Http_utils.is_req_from_root_host req
    then Lwt.return_ok Pool_database.root
    else tenant_database_label_of_request req
  in
  let languages_from_request ?contact req tenant_db =
    let%lwt tenant_languages = Settings.find_languages tenant_db in
    let query_language = find_query_language req in
    let bind_valid =
      CCFun.flip CCOption.bind (fun lang ->
        if CCList.mem ~eq:Pool_common.Language.equal lang tenant_languages
        then Some lang
        else None)
    in
    let user_language = function
      | Some (p : Contact.t) -> p.Contact.language |> Lwt.return
      | None ->
        let%lwt lang =
          Http_utils.user_from_session tenant_db req
          ||> CCOption.to_result Pool_common.Message.(NotFound Field.User)
          >>= fun user ->
          Contact.find tenant_db (user.Sihl_user.id |> Pool_common.Id.of_string)
          >|+ fun p -> p.Contact.language
        in
        CCResult.get_or lang ~default:None |> Lwt.return
    in
    (match query_language |> bind_valid with
     | Some language -> Lwt.return language
     | None ->
       user_language contact
       ||> bind_valid
       ||> CCOption.value
             ~default:
               (CCOption.get_exn_or
                  "Cannot determine language"
                  (CCList.head_opt tenant_languages)))
    ||> fun lang -> query_language, lang
  in
  let filter handler req =
    let csrf = Sihl.Web.Csrf.find_exn req in
    let message =
      CCOption.bind
        (Sihl.Web.Flash.find_alert req)
        Pool_common.Message.Collection.of_string
    in
    let find_user pool =
      Http_utils.user_from_session pool req
      >|> CCOption.map_or ~default:(Lwt.return Guest) (fun user ->
            if Sihl_user.is_admin user
            then user |> Admin.create |> admin |> Lwt.return
            else
              Contact.find_by_user pool user
              ||> CCResult.to_opt
              ||> CCOption.map_or ~default:Guest contact)
    in
    let%lwt context =
      let* database_label = database_label_of_request req in
      let%lwt user = find_user database_label in
      let%lwt query_lang, language =
        let admin = Lwt.return (None, Pool_common.Language.En) in
        if is_root
        then admin
        else (
          match user with
          | Admin _ -> admin
          | Contact _ | Guest -> languages_from_request req database_label)
      in
      create (query_lang, language, database_label, message, csrf, user)
      |> Lwt.return_ok
    in
    match context with
    | Ok context -> context |> set req |> handler
    | Error _ ->
      let open Http_utils in
      path_with_language (find_query_language req) "/error" |> redirect_to
  in
  Rock.Middleware.create ~name:"tenant.context" ~filter
;;
