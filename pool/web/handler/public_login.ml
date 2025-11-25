open CCFun.Infix
open Utils.Lwt_result.Infix
module HttpUtils = Http_utils
module Message = HttpUtils.Message
module Response = Http_response

let src = Logs.Src.create "handler.public.login"
let to_ctx = Database.to_ctx
let create_layout req = General.create_tenant_layout req

let increase_sign_in_count ~tags database_label user =
  let open Pool_context in
  let open Cqrs_command in
  let events =
    match user with
    | Admin admin -> Admin_command.UpdateSignInCount.handle ~tags admin
    | Contact contact -> Contact_command.UpdateSignInCount.handle ~tags contact
    | Guest -> Ok []
  in
  events |> Lwt_result.lift |>> Pool_event.handle_events database_label user
;;

let login_get req =
  let result context =
    Response.bad_request_render_error context
    @@
    let open Sihl.Web in
    Page.Public.login ?intended:(HttpUtils.find_intended_opt req) context
    |> create_layout req ~active_navigation:"/login" context
    >|+ Response.of_html
  in
  Response.handle ~src req result
;;

let login_token_confirmation req =
  let tags = Pool_context.Logger.Tags.req req in
  let open Response in
  let result ({ Pool_context.database_label; _ } as context) =
    let* user, auth, (_ : Authentication.Token.t) =
      Helpers_login.decode_2fa_confirmation database_label req ~tags
      |> bad_request_on_error login_get
    in
    Response.bad_request_render_error context
    @@ (Page.Public.login_token_confirmation
          ~authentication_id:auth.Authentication.id
          ?intended:(HttpUtils.find_intended_opt req)
          ~email:(Pool_user.email user)
          context
        |> create_layout req ~active_navigation:"/login" context
        >|+ Sihl.Web.Response.of_html)
  in
  Response.handle ~src req result
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
    let success () =
      Page.Public.login_token_confirmation
        ~authentication_id:auth.Authentication.id
        ?intended:(HttpUtils.find_intended_opt req)
        ~email:(Pool_user.email user)
        context
      |> create_layout req ~active_navigation:"/login" context
      >|+ Sihl.Web.Response.of_html
    in
    events |> handle_events >|> success
  in
  Response.handle ~src req result
;;

let login_cofirmation req =
  let open Response in
  let open HttpUtils in
  let tags = Pool_context.Logger.Tags.req req in
  let result { Pool_context.database_label; query_parameters; _ } =
    let* user, auth, token =
      Helpers_login.decode_2fa_confirmation database_label req ~tags
      |> bad_request_on_error login_get
    in
    bad_request_on_error login_token_confirmation
    @@
    let* user, events = Helpers_login.confirm_2fa_login ~tags user auth token req in
    let success_and_redirect
          ?(set_completion_cookie = false)
          ?redirect
          ?(actions = [])
          context_user
      =
      let redirect =
        let open CCOption in
        redirect
        <+> find_intended_opt req
        |> value ~default:(Pool_context.dashboard_path context_user)
        |> url_with_field_params query_parameters
      in
      let tags = Pool_context.Logger.Tags.req req in
      let%lwt () = Pool_event.handle_events database_label context_user events in
      let* () = increase_sign_in_count ~tags database_label context_user in
      redirect_to_with_actions
        redirect
        ([ Sihl.Web.Session.set [ "user_id", user.Pool_user.id |> Pool_user.Id.value ] ]
         @ actions)
      ||> (fun res ->
      if set_completion_cookie
      then
        Sihl.Web.Session.set_value ~key:Contact.profile_completion_cookie "true" req res
      else res)
      |> Lwt_result.ok
    in
    let handle_admin_login user =
      user.Pool_user.id
      |> Admin.(Id.of_user %> find database_label)
      >|+ Pool_context.admin
      >>= success_and_redirect
    in
    let handle_contact_login user =
      let* contact = user.Pool_user.id |> Contact.(Id.of_user %> find database_label) in
      let%lwt required_answers_given =
        Custom_field.all_required_answered database_label (Contact.id contact)
      in
      let contact = contact |> Pool_context.contact in
      match required_answers_given with
      | true -> success_and_redirect contact
      | false ->
        success_and_redirect
          ~set_completion_cookie:true
          ~redirect:"/user/completion"
          ~actions:[ Message.set ~error:[ Pool_message.Error.RequiredFieldsMissing ] ]
          contact
    in
    match user |> Pool_user.is_confirmed with
    | false ->
      redirect_to (url_with_field_params query_parameters "/email-confirmation")
      |> Lwt_result.ok
    | true ->
      user
      |> Admin.user_is_admin database_label
      >|> (function
       | true -> handle_admin_login user
       | false -> handle_contact_login user)
  in
  Response.handle ~src req result
;;

let request_reset_password_get req =
  let result context =
    Response.bad_request_render_error context
    @@
    let open Sihl.Web in
    Page.Public.request_reset_password context
    |> create_layout req ~active_navigation:"/request-reset-password" context
    >|+ Response.of_html
  in
  Response.handle ~src req result
;;

let request_reset_password_post req =
  let open HttpUtils in
  let open Cqrs_command.Common_command.ResetPassword in
  let open Message_template in
  let result ({ Pool_context.database_label; query_parameters; _ } as context) =
    let redirect_path =
      url_with_field_params query_parameters "/request-reset-password"
    in
    Response.bad_request_on_error request_reset_password_get
    @@
    let tags = Pool_context.Logger.Tags.req req in
    let tenant = Pool_context.Tenant.get_tenant_exn req in
    let tenant_languages = Pool_context.Tenant.get_tenant_languages_exn req in
    let* user =
      Sihl.Web.Request.to_urlencoded req
      ||> decode
      |>> Pool_user.find_active_by_email_opt database_label
    in
    let make_message message_language user =
      let email_address = Pool_user.email user in
      let reset_message () =
        PasswordReset.create database_label message_language (Tenant tenant) user
      in
      let verification_message () =
        let make_message token =
          let open Pool_user in
          Message_template.SignUpVerification.create
            database_label
            message_language
            tenant
            email_address
            token
            (firstname user)
            (lastname user)
            (id user)
        in
        let open Email in
        user
        |> Pool_user.email
        |> find_active_token database_label
        >|> function
        | None -> Lwt_result.fail Pool_message.(Error.NotFound Field.Token)
        | Some token -> make_message token |> Lwt_result.ok
      in
      let make_message = function
        | None -> verification_message ()
        | Some (_ : Pool_user.EmailVerified.t) -> reset_message ()
      in
      let open Pool_context in
      let%lwt user = context_user_of_user database_label user in
      match user with
      | Guest -> Lwt_result.fail Pool_message.(Error.NotFound Field.User)
      | Admin { Admin.email_verified; _ } -> make_message email_verified
      | Contact { Contact.email_verified; _ } -> make_message email_verified
    in
    let handle () =
      match user with
      | None -> Lwt_result.return ()
      | Some user ->
        let%lwt message_language =
          match Pool_context.Utils.query_language tenant_languages query_parameters with
          | Some lang -> Lwt.return lang
          | None ->
            (match%lwt Admin.user_is_admin database_label user with
             | true -> Lwt.return context.Pool_context.language
             | false ->
               Contact.find_by_user database_label user
               >|- Pool_common.Utils.failwith
               ||> CCResult.get_or_failwith
               ||> fun Contact.{ language; _ } ->
               CCOption.value ~default:context.Pool_context.language language)
        in
        make_message message_language user
        >== handle ~tags
        |>> Pool_event.handle_events ~tags database_label context.Pool_context.user
    in
    let redirect () =
      redirect_to_with_actions
        redirect_path
        [ Message.set ~success:[ Pool_message.Success.PasswordResetSuccessMessage ] ]
      >|> Lwt_result.return
    in
    handle ()
    ||> CCResult.get_lazy (fun err ->
      let (_ : Pool_message.Error.t) = Pool_common.Utils.with_log_error err in
      ())
    >|> redirect
  in
  Response.handle ~src req result
;;

let reset_password_get req =
  let result ({ Pool_context.database_label; language; _ } as context) =
    Response.bad_request_render_error context
    @@
    let token = Sihl.Web.Request.query Pool_message.Field.(Token |> show) req in
    let%lwt password_policy =
      I18n.find_by_key database_label I18n.Key.PasswordPolicyText language
    in
    match token with
    | None ->
      HttpUtils.redirect_to_with_actions
        "/request-reset-password/"
        [ Message.set ~error:[ Pool_message.(Error.NotFound Field.Token) ] ]
      |> Lwt_result.ok
    | Some token ->
      Page.Public.reset_password token context password_policy
      |> create_layout req ~active_navigation:"/reset-password" context
      >|+ Sihl.Web.Response.of_html
  in
  Response.handle ~src req result
;;

let reset_password_post req =
  let%lwt urlencoded = Sihl.Web.Request.to_urlencoded req in
  let result { Pool_context.database_label; user; _ }
    : (Rock.Response.t, Response.http_error) Lwt_result.t
    =
    let open Pool_message in
    let open HttpUtils in
    let open Cqrs_command.User_command.ResetPassword in
    let handle_error result =
      Lwt_result.ok
      @@
      match%lwt result with
      | Ok o -> Lwt.return o
      | Error (err, Some token) ->
        redirect_to_with_actions
          (url_with_field_params
             [ Field.Token, Pool_token.value token ]
             "/reset-password")
          [ Message.set ~error:[ err ] ]
      | Error (err, None) ->
        redirect_to_with_actions "/reset-password" [ Message.set ~error:[ err ] ]
    in
    handle_error
    @@
    let* ({ Cqrs_command.User_command.ResetPassword.token; _ } as decoded_params) =
      let open Cqrs_command.User_command.ResetPassword in
      let token =
        let open CCOption in
        CCList.assoc_opt ~eq:String.equal Field.(show Token) urlencoded
        >>= CCList.head_opt
        >|= Pool_token.of_string
      in
      let%lwt is_token_valid =
        match token with
        | Some token -> token |> Pool_token.is_valid database_label
        | None -> Lwt.return_false
      in
      urlencoded
      |> decode
      |> CCResult.map_err (fun err -> err, if is_token_valid then token else None)
      |> Lwt_result.lift
    in
    let handle_request decoded_params =
      Lwt_result.map_error (fun err -> err, Some token)
      @@
      let tags = Pool_context.Logger.Tags.req req in
      let* all_events =
        let* user_uuid =
          Pool_token.read database_label token ~k:"user_id"
          ||> CCOption.to_result Pool_message.(Error.Invalid Field.Token)
          >|+ Pool_user.Id.of_string
        in
        let* reset_password_events = handle ~tags decoded_params |> Lwt_result.lift in
        let* import_events =
          User_import.find_pending_by_user_id_opt database_label user_uuid
          >|> function
          | None -> Lwt.return_ok []
          | Some import ->
            Pool_user.find_exn database_label user_uuid
            >|> Pool_context.context_user_of_user database_label
            ||> fun user ->
            Cqrs_command.User_import_command.DisableImport.handle ~tags (user, import)
        in
        Lwt.return_ok (reset_password_events @ import_events)
      in
      let%lwt () = Pool_event.handle_events database_label user all_events in
      redirect_to_with_actions "/login" [ Message.set ~success:[ Success.PasswordReset ] ]
      >|> Lwt.return_ok
    in
    handle_request decoded_params
  in
  Response.handle ~src req result
;;

let logout req =
  HttpUtils.(
    redirect_to_with_actions
      (HttpUtils.retain_url_params req "/login" |> Uri.to_string)
      [ Sihl.Web.Session.set [ "user_id", "" ] ])
;;
