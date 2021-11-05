module HttpUtils = Http_utils
module Message = HttpUtils.Message

let dashboard_path tenant_db user =
  let%lwt is_admin = Admin.user_is_admin tenant_db user in
  Lwt.return
  @@
  match is_admin with
  | true -> "/admin/dashboard"
  | false -> "/participant/dashboard"
;;

let login_get req =
  let%lwt result =
    let open Lwt_result.Syntax in
    let* tenant_db = Middleware.Tenant.tenant_db_of_request req in
    let%lwt user =
      Service.User.Web.user_from_session
        ~ctx:[ "pool", tenant_db |> Pool_common.Database.Label.value ]
        req
    in
    match user with
    | Some user ->
      let%lwt dashboard = dashboard_path tenant_db user in
      dashboard
      |> Sihl.Web.externalize_path
      |> Sihl.Web.Response.redirect_to
      |> Lwt.return_ok
    | None ->
      let csrf = HttpUtils.find_csrf req in
      let message =
        Sihl.Web.Flash.find_alert req |> CCFun.flip CCOpt.bind Message.of_string
      in
      Page.Public.login csrf message ()
      |> Sihl.Web.Response.of_html
      |> Lwt.return_ok
  in
  result
  |> CCResult.map_err (fun err -> err, "/")
  |> HttpUtils.extract_happy_path
;;

let login_post req =
  let%lwt urlencoded = Sihl.Web.Request.to_urlencoded req in
  let%lwt result =
    let open Lwt_result.Syntax in
    Lwt_result.map_err (fun err -> err, "/login")
    @@ let* params =
         HttpUtils.urlencoded_to_params urlencoded [ "email"; "password" ]
         |> CCOpt.to_result Pool_common.Error.LoginProvideDetails
         |> Lwt_result.lift
       in
       let* tenant_db = Middleware.Tenant.tenant_db_of_request req in
       let email = List.assoc "email" params in
       let password = List.assoc "password" params in
       let* user =
         Service.User.login
           ~ctx:[ "pool", tenant_db |> Pool_common.Database.Label.value ]
           email
           ~password
         |> Lwt_result.map_err Pool_common.Error.handle_sihl_login_error
       in
       let%lwt success_path = dashboard_path tenant_db user in
       HttpUtils.redirect_to_with_actions
         success_path
         [ Sihl.Web.Session.set [ "user_id", user.Sihl_user.id ] ]
       |> Lwt_result.ok
  in
  result |> HttpUtils.extract_happy_path
;;

let request_reset_password_get req =
  let%lwt result =
    let open Lwt_result.Syntax in
    let* tenant_db = Middleware.Tenant.tenant_db_of_request req in
    let%lwt user =
      Service.User.Web.user_from_session
        ~ctx:[ "pool", tenant_db |> Pool_common.Database.Label.value ]
        req
    in
    match user with
    | Some user ->
      let%lwt dashboard = dashboard_path tenant_db user in
      dashboard
      |> Sihl.Web.externalize_path
      |> Sihl.Web.Response.redirect_to
      |> Lwt.return_ok
    | None ->
      let csrf = HttpUtils.find_csrf req in
      let message =
        Sihl.Web.Flash.find_alert req |> CCFun.flip CCOpt.bind Message.of_string
      in
      Page.Public.request_reset_password csrf message ()
      |> Sihl.Web.Response.of_html
      |> Lwt.return
      |> Lwt_result.ok
  in
  result
  |> CCResult.map_err (fun err -> err, "/")
  |> HttpUtils.extract_happy_path
;;

let request_reset_password_post req =
  let%lwt result =
    let open Lwt_result.Syntax in
    let open Utils.Lwt_result.Infix in
    let* email =
      Sihl.Web.Request.urlencoded "email" req
      |> Lwt.map (CCOpt.to_result Pool_common.Error.(NotFound Email))
    in
    let* tenant_db = Middleware.Tenant.tenant_db_of_request req in
    let ctx = [ "pool", tenant_db |> Pool_common.Database.Label.value ] in
    let* user =
      Service.User.find_by_email_opt ~ctx email
      |> Lwt.map (CCOpt.to_result Pool_common.Error.PasswordResetMessage)
    in
    Common_user.Event.Email.PasswordReset.create tenant_db ~user
    >|= Service.Email.send ~ctx
  in
  match result with
  | Ok _ | Error _ ->
    HttpUtils.redirect_to_with_actions
      "/request-reset-password"
      [ Message.set
          ~success:[ Pool_common.Error.(PasswordResetMessage |> message) ]
      ]
;;

let reset_password_get req =
  let token = Sihl.Web.Request.query "token" req in
  match token with
  | None ->
    HttpUtils.redirect_to_with_actions
      "/request-reset-password/"
      [ Message.set ~error:[ Pool_common.Error.(NotFound Token |> message) ] ]
  | Some token ->
    let csrf = HttpUtils.find_csrf req in
    let message =
      Sihl.Web.Flash.find_alert req |> CCFun.flip CCOpt.bind Message.of_string
    in
    Page.Public.reset_password csrf message token ()
    |> Sihl.Web.Response.of_html
    |> Lwt.return
;;

let reset_password_post req =
  let%lwt urlencoded = Sihl.Web.Request.to_urlencoded req in
  let%lwt result =
    let open Lwt_result.Syntax in
    let* params =
      HttpUtils.urlencoded_to_params
        urlencoded
        [ "token"; "password"; "password_confirmation" ]
      |> CCOpt.to_result
           (Pool_common.Error.PasswordResetInvalidData, "/reset-password/")
      |> Lwt_result.lift
    in
    let go = CCFun.flip List.assoc params in
    let token = go "token" in
    let password = go "password" in
    let password_confirmation = go "password_confirmation" in
    let* () =
      Lwt_result.map_err (fun err ->
          err, Format.asprintf "/reset-password/?token=%s" token)
      @@ let* tenant_db = Middleware.Tenant.tenant_db_of_request req in
         Service.PasswordReset.reset_password
           ~ctx:[ "pool", tenant_db |> Pool_common.Database.Label.value ]
           ~token
           ~password
           ~password_confirmation
         |> Lwt_result.map_err
              (CCFun.const Pool_common.Error.passwordresetinvaliddata)
    in
    HttpUtils.redirect_to_with_actions
      "/login"
      [ Message.set
          ~success:[ Pool_common.Error.(PasswordResetFinish |> message) ]
      ]
    |> Lwt_result.ok
  in
  HttpUtils.extract_happy_path result
;;

let logout _ =
  HttpUtils.redirect_to_with_actions
    "/login"
    [ Sihl.Web.Session.set [ "user_id", "" ] ]
;;
