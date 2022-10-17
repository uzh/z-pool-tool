let context context () =
  let tenant_db_of_request req
    : (Pool_database.Label.t, Pool_common.Message.error) result Lwt.t
    =
    (* TODO handle PREFIX_PATH of Tenant URLs, multiple tenants behind the same
       host cannot be handled at the moment *)
    let open Lwt_result.Syntax in
    let* host =
      req
      |> Sihl.Web.Request.header "host"
      |> CCOption.to_result Pool_common.Message.(NotFound Field.Host)
      |> Lwt_result.lift
    in
    let%lwt selections = Pool_tenant.Selection.find_all () in
    CCList.assoc_opt
      ~eq:(fun m k -> CCString.prefix ~pre:m k)
      host
      (selections
      |> CCList.map (fun sel -> Pool_tenant.Selection.(url sel, label sel)))
    |> CCOption.to_result Pool_common.Message.(NotFound Field.TenantPool)
    |> CCResult.map_err (CCFun.const Pool_common.Message.SessionTenantNotFound)
    |> Lwt_result.lift
  in
  let language_from_request ?contact req tenant_db =
    let open CCOption in
    let%lwt tenant_languages = Settings.find_languages tenant_db in
    let is_valid lang =
      match CCList.mem ~eq:Pool_common.Language.equal lang tenant_languages with
      | true -> Some lang
      | false -> None
    in
    let user_language =
      let open Utils.Lwt_result.Infix in
      function
      | Some (p : Contact.t) -> p.Contact.language |> Lwt.return
      | None ->
        let%lwt lang =
          Http_utils.user_from_session tenant_db req
          ||> CCOption.to_result Pool_common.Message.(NotFound Field.User)
          >>= fun user ->
          Contact.find tenant_db (user.Sihl_user.id |> Pool_common.Id.of_string)
          >|= fun p -> p.Contact.language
        in
        CCResult.get_or lang ~default:None |> Lwt.return
    in
    let query_language = Http_utils.find_query_lang req >>= is_valid in
    query_language
    |> function
    | Some lang -> Lwt.return lang
    | None ->
      user_language contact
      |> Lwt.map (fun l ->
           l
           >>= is_valid
           |> value
                ~default:
                  (CCOption.get_exn_or
                     "Cannot determine language"
                     (CCList.head_opt tenant_languages)))
  in
  let filter handler req =
    let open Lwt_result.Syntax in
    let query_lang = Http_utils.find_query_lang req in
    let csrf = Sihl.Web.Csrf.find_exn req in
    let message =
      CCOption.bind
        (Sihl.Web.Flash.find_alert req)
        Pool_common.Message.Collection.of_string
    in
    let find_tenant_user pool =
      let open Utils.Lwt_result.Infix in
      let%lwt user =
        Service.User.Web.user_from_session ~ctx:(Pool_tenant.to_ctx pool) req
      in
      match user with
      | None -> Lwt.return_none
      | Some user ->
        let open Pool_context in
        (match%lwt Admin.user_is_admin pool user with
         | false ->
           user
           |> Contact.find_by_user pool
           ||> CCResult.to_opt
           ||> CCOption.map contact
         | true -> user |> admin |> Lwt.return_some)
    in
    let is_root_request = Http_utils.is_req_from_root_host req in
    let%lwt context =
      match is_root_request with
      | true ->
        let pool = Pool_database.root in
        let%lwt user =
          let open Lwt.Infix in
          Service.User.Web.user_from_session ~ctx:(Pool_tenant.to_ctx pool) req
          >|= CCFun.flip CCOption.bind (fun user ->
                if Sihl_user.is_admin user
                then Some (Pool_context.root user)
                else None)
        in
        Pool_context.create
          (None, Pool_common.Language.En, pool, message, csrf, user)
        |> Lwt_result.return
      | false ->
        let* tenant_db = tenant_db_of_request req in
        let%lwt user = find_tenant_user tenant_db in
        (match context with
         | `Admin ->
           Pool_context.create
             (None, Pool_common.Language.En, tenant_db, message, csrf, user)
           |> Lwt_result.return
         | `Contact ->
           let%lwt language = language_from_request req tenant_db in
           Pool_context.create
             (query_lang, language, tenant_db, message, csrf, user)
           |> Lwt_result.return)
    in
    match context with
    | Ok context -> context |> Pool_context.set req |> handler
    | Error _ ->
      Http_utils.(path_with_language query_lang "/error" |> redirect_to)
  in
  Rock.Middleware.create ~name:"tenant.context" ~filter
;;
