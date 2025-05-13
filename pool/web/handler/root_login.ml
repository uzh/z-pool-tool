open Pool_message
open Utils.Lwt_result.Infix
module HttpUtils = Http_utils
module Message = HttpUtils.Message
module Response = Http_response

let src = Logs.Src.create "handler.root.login"
let root_login_path = "/root/login"
let root_entrypoint_path = Http_utils.Url.Root.pool_path ()
let redirect_to_entrypoint = HttpUtils.redirect_to root_entrypoint_path

let login_get req =
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
  Response.handle ~src req result
;;

let render_token_confirmation auth user context req =
  let open Sihl.Web in
  Page.Root.Login.token_confirmation
    ~authentication_id:auth.Authentication.id
    ?intended:(HttpUtils.find_intended_opt req)
    ~email:(Pool_user.email user)
    context
  |> General.create_root_layout ~active_navigation:"/root/login" context
  ||> Response.of_html
;;

let login_post req =
  let tags = Pool_context.Logger.Tags.req req in
  let%lwt urlencoded = Sihl.Web.Request.to_urlencoded req in
  let result ({ Pool_context.database_label; user; _ } as context) =
    Response.bad_request_on_error ~urlencoded login_get
    @@
    let handle_events = Pool_event.handle_events database_label user in
    let* user, auth, events =
      Helpers_login.create_2fa_login ~tags req context urlencoded
    in
    let success () = render_token_confirmation auth user context req in
    events |> handle_events >|> success |> Lwt_result.ok
  in
  Response.handle ~src req result
;;

let confirmation_post req =
  let open Response in
  let tags = Pool_context.Logger.Tags.req req in
  let result ({ Pool_context.database_label; user; _ } as context) =
    let handle_events = Pool_event.handle_events database_label user in
    let* user, auth, token =
      Helpers_login.decode_2fa_confirmation database_label req ~tags
      |> bad_request_on_error login_get
    in
    let* user, events =
      Helpers_login.confirm_2fa_login ~tags user auth token req
      |> bad_request_on_error (render_token_confirmation auth user context)
    in
    let success () =
      HttpUtils.redirect_to_with_actions
        root_entrypoint_path
        [ Sihl.Web.Session.set [ "user_id", user.Pool_user.id |> Pool_user.Id.value ] ]
    in
    events |> handle_events >|> success |> Lwt_result.ok
  in
  handle ~src req result
;;

let request_reset_password_get req =
  let result context =
    Response.bad_request_render_error context
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
  Response.handle ~src req result
;;

let request_reset_password_post req =
  let open Utils.Lwt_result.Infix in
  let open HttpUtils in
  let open Cqrs_command.Common_command.ResetPassword in
  let open Message_template in
  let%lwt urlencoded = Sihl.Web.Request.to_urlencoded req in
  let result { Pool_context.database_label; language; user; _ } =
    Response.bad_request_on_error ~urlencoded request_reset_password_get
    @@
    let tags = Pool_context.Logger.Tags.req req in
    urlencoded
    |> decode
    |> Lwt_result.lift
    >>= (fun email ->
    Pool_user.find_active_by_email_opt database_label email
    ||> CCOption.to_result Error.PasswordResetFailMessage)
    >>= PasswordReset.create database_label language Root
    >>= CCFun.(handle ~tags %> Lwt_result.lift)
    |>> Pool_event.handle_events ~tags database_label user
    >|> function
    | Ok () | Error (_ : Error.t) ->
      redirect_to_with_actions
        "/root/request-reset-password"
        [ Message.set ~success:[ Success.PasswordResetSuccessMessage ] ]
      >|> Lwt.return_ok
  in
  Response.handle ~src req result
;;

let reset_password_get req =
  let result context =
    let open Utils.Lwt_result.Infix in
    Response.bad_request_render_error context
    @@ let* token =
         Sihl.Web.Request.query Field.(Token |> show) req
         |> CCOption.to_result (Error.NotFound Field.Token)
         |> Lwt_result.lift
       in
       Page.Root.Login.reset_password token context
       |> General.create_root_layout ~active_navigation:"/root/reset-password" context
       ||> Sihl.Web.Response.of_html
       |> Lwt_result.ok
  in
  Response.handle ~src req result
;;

let reset_password_post req =
  let%lwt urlencoded = Sihl.Web.Request.to_urlencoded req in
  let result _ =
    let open Utils.Lwt_result.Infix in
    Response.bad_request_on_error reset_password_get
    @@
    let* params =
      Field.[ Token; Password; PasswordConfirmation ]
      |> CCList.map Field.show
      |> HttpUtils.urlencoded_to_params urlencoded
      |> CCOption.to_result Error.PasswordResetInvalidData
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
      >|- fun (_ : Pool_message.Error.t) -> Error.PasswordResetInvalidData
    in
    HttpUtils.redirect_to_with_actions
      root_login_path
      [ Message.set ~success:[ Success.PasswordReset ] ]
    |> Lwt_result.ok
  in
  Response.handle ~src req result
;;

let logout _ =
  HttpUtils.redirect_to_with_actions
    root_login_path
    [ Sihl.Web.Session.set [ "user_id", "" ] ]
;;
