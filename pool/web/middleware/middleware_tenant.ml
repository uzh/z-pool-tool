let tenant_db_of_request req
    : (Pool_database.Label.t, Pool_common.Message.error) result Lwt.t
  =
  (* TODO handle PREFIX_PATH of Tenant URLs, multiple tenants behind the same
     host cannot be handled at the moment *)
  let open Lwt_result.Syntax in
  let* host =
    req
    |> Sihl.Web.Request.header "host"
    |> CCOption.to_result Pool_common.Message.(NotFound Host)
    |> Lwt_result.lift
  in
  let%lwt selections = Pool_tenant.Selection.find_all () in
  CCList.assoc_opt
    ~eq:(fun m k -> CCString.prefix ~pre:m k)
    host
    (selections
    |> CCList.map (fun sel -> Pool_tenant.Selection.(url sel, label sel)))
  |> CCOption.to_result Pool_common.Message.(NotFound TenantPool)
  |> CCResult.map_err (CCFun.const Pool_common.Message.SessionTenantNotFound)
  |> Lwt_result.lift
;;

let tenant_context ?(is_admin = false) () =
  let language_from_request ?participant req tenant_db =
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
      | Some (p : Participant.t) -> p.Participant.language |> Lwt.return
      | None ->
        let%lwt lang =
          Http_utils.user_from_session tenant_db req
          ||> CCOption.to_result Pool_common.Message.(NotFound User)
          >>= fun user ->
          Participant.find
            tenant_db
            (user.Sihl_user.id |> Pool_common.Id.of_string)
          >|= fun p -> p.Participant.language
        in
        CCResult.get_or lang ~default:None |> Lwt.return
    in
    let query_language = Http_utils.find_query_lang req >>= is_valid in
    query_language
    |> function
    | Some lang -> Lwt.return lang
    | None ->
      user_language participant
      |> Lwt.map
           (value
              ~default:
                (CCOption.get_exn_or
                   "Cannot determine language"
                   (CCList.head_opt tenant_languages)))
  in
  let filter handler req =
    let query_lang = Http_utils.find_query_lang req in
    let%lwt tenant_db = tenant_db_of_request req in
    match tenant_db with
    | Error _ ->
      Http_utils.path_with_language query_lang "/error"
      |> Http_utils.redirect_to
    | Ok tenant_db ->
      let%lwt query_lang, language =
        match is_admin with
        | true -> Lwt.return (None, Pool_common.Language.En)
        | false ->
          let%lwt language = language_from_request req tenant_db in
          Lwt.return (Http_utils.find_query_lang req, language)
      in
      Pool_tenant.Context.create query_lang language tenant_db
      |> Pool_tenant.Context.set req
      |> handler
  in
  Rock.Middleware.create ~name:"tenant.context" ~filter
;;
