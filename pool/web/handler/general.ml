let user_from_session db_pool req : Sihl_user.t option Lwt.t =
  let ctx = Pool_tenant.to_ctx db_pool in
  Service.User.Web.user_from_session ~ctx req
;;

let dashboard_path tenant_db query_lang user =
  let open Lwt.Infix in
  Admin.user_is_admin tenant_db user
  >|= (function
        | true -> "/admin/dashboard"
        | false -> "/dashboard")
  >|= Http_utils.path_with_language query_lang
;;

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
        user_from_session tenant_db req
        ||> CCOption.to_result Pool_common.Message.(NotFound User)
        >>= fun user ->
        Participant.find
          tenant_db
          (user.Sihl_user.id |> Pool_common.Id.of_string)
        >|= fun p -> p.Participant.language
      in
      CCResult.get_or lang ~default:None |> Lwt.return
  in
  Http_utils.find_query_lang req
  >>= is_valid
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
;;
