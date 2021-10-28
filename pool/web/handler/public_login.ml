module HttpUtils = Http_utils
module Message = HttpUtils.Message

let user_error_to_string = function
  | `Incorrect_password | `Does_not_exist -> "Invalid password provided"
;;

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
    let* tenant_db = Middleware.Tenant_middleware.tenant_db_of_request req in
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
    let login_path = "/login" in
    let* params =
      HttpUtils.urlencoded_to_params urlencoded [ "email"; "password" ]
      |> CCOpt.to_result ("Please provide email and password", login_path)
      |> Lwt_result.lift
    in
    let* tenant_db =
      Middleware.Tenant_middleware.tenant_db_of_request req
      |> Lwt_result.map_err (fun err -> err, login_path)
    in
    let email = List.assoc "email" params in
    let password = List.assoc "password" params in
    let* user =
      Service.User.login
        ~ctx:[ "pool", tenant_db |> Pool_common.Database.Label.value ]
        email
        ~password
      |> Lwt_result.map_err (fun user_result ->
             user_error_to_string user_result, login_path)
    in
    let%lwt success_path = dashboard_path tenant_db user in
    Lwt_result.ok
    @@ HttpUtils.redirect_to_with_actions
         success_path
         [ Sihl.Web.Session.set [ "user_id", user.Sihl_user.id ] ]
  in
  result |> HttpUtils.extract_happy_path
;;

let request_reset_password_get req =
  let%lwt result =
    let open Lwt_result.Syntax in
    let* tenant_db = Middleware.Tenant_middleware.tenant_db_of_request req in
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
  let open Utils.Lwt_result.Infix in
  let reset_path = Sihl.Web.externalize_path "/request-reset-password" in
  let message =
    "You will receive an email with a link to reset your password if an \
     account with the provided email is existing."
  in
  let%lwt email =
    Sihl.Web.Request.urlencoded "email" req |> Lwt.map Option.get
  in
  let user tenant_db email =
    Service.User.find_by_email_opt
      ~ctx:[ "pool", tenant_db |> Pool_common.Database.Label.value ]
      email
    |> Lwt.map (Option.to_result ~none:message)
  in
  let create_reset_email tenant_db user =
    Email.PasswordReset.create tenant_db message ~user
  in
  let send_mail email = email |> Service.Email.send |> Lwt_result.ok in
  let show_info =
    Lwt_result.ok
    @@ HttpUtils.redirect_to_with_actions
         reset_path
         [ Message.set ~success:[ message ] ]
  in
  let%lwt result =
    let open Lwt_result.Syntax in
    let* tenant_db =
      Middleware.Tenant_middleware.tenant_db_of_request req
      |> Lwt_result.map_err (fun err -> err, message)
    in
    email
    |> user tenant_db
    >>= create_reset_email tenant_db
    >>= send_mail
    >> show_info
    |> Lwt_result.map_err (fun err -> err, reset_path)
  in
  HttpUtils.extract_happy_path_generic result (fun _ ->
      Message.set ~warning:[] ~success:[ message ] ~info:[] ~error:[])
;;

let reset_password_get req =
  let token = Sihl.Web.Request.query "token" req in
  match token with
  | None ->
    HttpUtils.redirect_to_with_actions
      "/request-reset-password/"
      [ Message.set ~error:[ "No password reset token found" ] ]
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
           ("Invalid token or password provided", "/reset-password/")
      |> Lwt_result.lift
    in
    let go = CCFun.flip List.assoc params in
    let token = go "token" in
    let password = go "password" in
    let password_confirmation = go "password_confirmation" in
    let* () =
      let open Lwt_result.Infix in
      let tenant_db req =
        Middleware.Tenant_middleware.tenant_db_of_request req
        |> Lwt_result.map_err (fun err ->
               err, Format.asprintf "/reset-password/?token=%s" token)
      in
      let reset db =
        Service.PasswordReset.reset_password
          ~ctx:[ "pool", db |> Pool_common.Database.Label.value ]
          ~token
          ~password
          ~password_confirmation
        |> Lwt_result.map_err (fun err ->
               err, Format.asprintf "/reset-password/?token=%s" token)
      in
      req |> tenant_db >>= reset
    in
    Lwt_result.ok
    @@ HttpUtils.redirect_to_with_actions
         "/login/"
         [ Message.set ~success:[ "Password reset, you can now log in." ] ]
  in
  HttpUtils.extract_happy_path result
;;

let logout _ =
  HttpUtils.redirect_to_with_actions
    "/login"
    [ Sihl.Web.Session.set [ "user_id", "" ] ]
;;
