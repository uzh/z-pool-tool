open Pool_message
module HttpUtils = Http_utils
module Message = HttpUtils.Message

let src = Logs.Src.create "handler.root.login"
let ctx = Database.(to_ctx root)
let root_login_path = "/root/login"
let root_entrypoint_path = "/root/tenants"
let redirect_to_entrypoint = HttpUtils.redirect_to root_entrypoint_path

let login_get req =
  let open Utils.Lwt_result.Infix in
  let result context =
    Pool_user.Web.user_from_session Database.root req
    >|> function
    | Some _ -> redirect_to_entrypoint |> Lwt_result.ok
    | None ->
      Logs.info (fun m ->
        m ~tags:(Pool_context.Logger.Tags.req req) "User not found in session");
      let open Sihl.Web in
      Page.Root.Login.login ?intended:(HttpUtils.find_intended_opt req) context
      |> General.create_root_layout ~active_navigation:"/root/login" context
      ||> Response.of_html
      |> Lwt_result.ok
  in
  result |> HttpUtils.extract_happy_path ~src req
;;

let login_post req =
  let%lwt urlencoded = Sihl.Web.Request.to_urlencoded req in
  let result { Pool_context.database_label; _ } =
    Utils.Lwt_result.map_error (fun err -> err, root_login_path)
    @@
    let open Utils.Lwt_result.Infix in
    let* user = Helpers.Login.login req urlencoded database_label in
    HttpUtils.redirect_to_with_actions
      root_entrypoint_path
      [ Sihl.Web.Session.set [ "user_id", user.Sihl_user.id ] ]
    |> Lwt_result.ok
  in
  result |> HttpUtils.extract_happy_path ~src req
;;

let request_reset_password_get req =
  let result context =
    Utils.Lwt_result.map_error (fun err -> err, root_entrypoint_path)
    @@
    let open Utils.Lwt_result.Infix in
    let open Sihl.Web in
    Pool_user.Web.user_from_session Database.root req
    >|> function
    | Some _ -> redirect_to_entrypoint |> Lwt_result.ok
    | None ->
      Page.Root.Login.request_reset_password context
      |> General.create_root_layout
           ~active_navigation:"/root/request-reset-password"
           context
      ||> Response.of_html
      |> Lwt_result.ok
  in
  result |> HttpUtils.extract_happy_path ~src req
;;

let request_reset_password_post req =
  let open HttpUtils in
  let open Cqrs_command.Common_command.ResetPassword in
  let open Message_template in
  let result { Pool_context.database_label; language; _ } =
    let open Utils.Lwt_result.Infix in
    let tags = Pool_context.Logger.Tags.req req in
    let redirect_path = "/root/request-reset-password" in
    Sihl.Web.Request.to_urlencoded req
    ||> decode
    >>= (fun email ->
          Pool_user.find_active_user_by_email_opt database_label email
          ||> CCOption.to_result Error.PasswordResetFailMessage)
    >>= PasswordReset.create database_label language Root
    >>= CCFun.(handle ~tags %> Lwt_result.lift)
    |>> Pool_event.handle_events ~tags database_label
    >|> function
    | Ok () | Error (_ : Error.t) ->
      redirect_to_with_actions
        redirect_path
        [ Message.set ~success:[ Success.PasswordResetSuccessMessage ] ]
      >|> Lwt.return_ok
  in
  result |> extract_happy_path_with_actions ~src req
;;

let reset_password_get req =
  let result context =
    let open Sihl.Web in
    let open Utils.Lwt_result.Infix in
    Utils.Lwt_result.map_error (fun err -> err, "/root/request-reset-password/")
    @@ let* token =
         Request.query Field.(Token |> show) req
         |> CCOption.to_result (Error.NotFound Field.Token)
         |> Lwt_result.lift
       in
       Page.Root.Login.reset_password token context
       |> General.create_root_layout
            ~active_navigation:"/root/reset-password"
            context
       ||> Response.of_html
       |> Lwt_result.ok
  in
  result |> HttpUtils.extract_happy_path ~src req
;;

let reset_password_post req =
  let%lwt urlencoded = Sihl.Web.Request.to_urlencoded req in
  let result _ =
    let open Utils.Lwt_result.Infix in
    let* params =
      Field.[ Token; Password; PasswordConfirmation ]
      |> CCList.map Field.show
      |> HttpUtils.urlencoded_to_params urlencoded
      |> CCOption.to_result
           (Error.PasswordResetInvalidData, "/root/reset-password/")
      |> Lwt_result.lift
    in
    let go field = field |> Field.show |> CCFun.flip List.assoc params in
    let token = go Field.Token in
    let* () =
      Pool_user.PasswordReset.reset_password
        ~ctx
        ~token
        (go Field.Password)
        (go Field.PasswordConfirmation)
      >|- fun (_ : string) ->
      ( Error.PasswordResetInvalidData
      , Format.asprintf "/root/reset-password/?token=%s" token )
    in
    HttpUtils.redirect_to_with_actions
      root_login_path
      [ Message.set ~success:[ Success.PasswordReset ] ]
    |> Lwt_result.ok
  in
  result |> HttpUtils.extract_happy_path ~src req
;;

let logout _ =
  HttpUtils.redirect_to_with_actions
    root_login_path
    [ Sihl.Web.Session.set [ "user_id", "" ] ]
;;
