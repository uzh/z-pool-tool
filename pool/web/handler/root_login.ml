open Pool_message
module HttpUtils = Http_utils
module Message = HttpUtils.Message

let src = Logs.Src.create "handler.root.login"
let root_login_path = "/root/login"
let root_entrypoint_path = "/root/pools"
let redirect_to_entrypoint = HttpUtils.redirect_to root_entrypoint_path

let login_get req =
  let open Utils.Lwt_result.Infix in
  let result context =
    Pool_user.Web.user_from_session Database.Pool.Root.label req
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
      [ Sihl.Web.Session.set [ "user_id", user.Pool_user.id |> Pool_user.Id.value ] ]
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
    Pool_user.Web.user_from_session Database.Pool.Root.label req
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
  let result { Pool_context.database_label; language; user; _ } =
    let open Utils.Lwt_result.Infix in
    let tags = Pool_context.Logger.Tags.req req in
    let redirect_path = "/root/request-reset-password" in
    Sihl.Web.Request.to_urlencoded req
    ||> decode
    >>= (fun email ->
    Pool_user.find_active_by_email_opt database_label email
    ||> CCOption.to_result Error.PasswordResetFailMessage)
    >>= PasswordReset.create database_label language Root
    >>= CCFun.(handle ~tags %> Lwt_result.lift)
    |>> Pool_event.handle_events ~tags database_label user
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
       |> General.create_root_layout ~active_navigation:"/root/reset-password" context
       ||> Response.of_html
       |> Lwt_result.ok
  in
  result |> HttpUtils.extract_happy_path ~src req
;;

let reset_password_post req =
  let%lwt urlencoded = Sihl.Web.Request.to_urlencoded req in
  let result _ =
    let open Utils.Lwt_result.Infix in
    let redirect = "/root/reset-password/" in
    let* params =
      Field.[ Token; Password; PasswordConfirmation ]
      |> CCList.map Field.show
      |> HttpUtils.urlencoded_to_params urlencoded
      |> CCOption.to_result (Error.PasswordResetInvalidData, redirect)
      |> Lwt_result.lift
    in
    let go field = field |> Field.show |> CCFun.flip (CCList.assoc ~eq:( = )) params in
    let token = go Field.Token in
    let password = Field.Password |> go |> Pool_user.Password.Plain.create in
    let password_confirmed =
      let open Pool_user.Password.Confirmation in
      Field.PasswordConfirmation |> go |> create
    in
    let* () =
      Pool_user.Password.Reset.reset_password
        Database.Pool.Root.label
        ~token
        password
        password_confirmed
      >|- fun (_ : Pool_message.Error.t) ->
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
