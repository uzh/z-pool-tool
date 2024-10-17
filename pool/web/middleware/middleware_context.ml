open CCFun

let context () =
  let open Utils.Lwt_result.Infix in
  let open Pool_context in
  let find_query_language = Http_utils.find_query_lang in
  let database_label_of_request is_root req =
    let tenant_database_label_of_request req =
      match Tenant.find req with
      | Ok { Tenant.tenant; _ } -> Ok tenant.Pool_tenant.database_label
      | Error _ -> Error Pool_message.(Error.Missing Field.Context)
    in
    if is_root then Ok Database.root else tenant_database_label_of_request req
  in
  let languages_from_request ?contact req tenant_db =
    let%lwt tenant_languages = Settings.find_languages tenant_db in
    let query_language = find_query_language req in
    let bind_valid =
      flip CCOption.bind (fun lang ->
        if CCList.mem ~eq:Pool_common.Language.equal lang tenant_languages
        then Some lang
        else None)
    in
    let user_language = function
      | Some (p : Contact.t) -> p.Contact.language |> Lwt.return
      | None ->
        let%lwt lang =
          Http_utils.user_from_session tenant_db req
          ||> CCOption.to_result Pool_message.(Error.NotFound Field.User)
          >>= fun user ->
          user.Pool_user.id
          |> Contact.(Id.of_user %> find tenant_db)
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
    let is_root = Http_utils.is_req_from_root_host req in
    let csrf = Sihl.Web.Csrf.find_exn req in
    let message =
      CCOption.bind
        (Sihl.Web.Flash.find_alert req)
        Pool_message.Collection.of_string
    in
    let find_user pool =
      Http_utils.user_from_session pool req
      >|> CCOption.map_or
            ~default:(Lwt.return Guest)
            (context_user_of_user pool)
    in
    let%lwt context =
      let* database_label =
        database_label_of_request is_root req |> Lwt_result.lift
      in
      let%lwt user = find_user database_label in
      let%lwt query_lang, language, guardian =
        let to_actor = Admin.id %> Guard.Uuid.actor_of Admin.Id.value in
        let combine roles = Lwt.return (None, Pool_common.Language.En, roles) in
        match user with
        | Admin admin ->
          to_actor admin
          |> Guard.Persistence.ActorRole.permissions_of_actor database_label
          >|> combine
        | (Guest | Contact _) when is_root -> combine []
        | Contact _ | Guest ->
          let%lwt query_lang, language =
            languages_from_request req database_label
          in
          Lwt.return (query_lang, language, [])
      in
      let%lwt announcement =
        match is_root with
        | true -> Lwt.return_none
        | false ->
          let context =
            match user with
            | Admin admin -> Some (`Admin, Admin.(admin |> id |> Id.to_common))
            | Contact contact ->
              Some (`Contact, Contact.(contact |> id |> Id.to_common))
            | Guest -> None
          in
          context
          |> CCOption.map_or
               ~default:Lwt.return_none
               (Announcement.find_by_user database_label)
      in
      create
        ( query_lang
        , language
        , database_label
        , message
        , csrf
        , user
        , guardian
        , announcement )
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
