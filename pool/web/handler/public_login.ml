module HttpUtils = Http_utils
module Message = HttpUtils.Message

let to_ctx = Pool_tenant.to_ctx

let redirect_to_dashboard tenant_db user =
  let open Lwt.Infix in
  General.dashboard_path tenant_db None user >>= HttpUtils.redirect_to
;;

let login_get req =
  let result context =
    let query_lang = context.Pool_tenant.Context.query_language in
    Lwt_result.map_err (fun err ->
        err, HttpUtils.path_with_language query_lang "/index")
    @@
    let tenant_db = context.Pool_tenant.Context.tenant_db in
    let%lwt user =
      Service.User.Web.user_from_session ~ctx:(to_ctx tenant_db) req
    in
    match user with
    | Some user -> redirect_to_dashboard tenant_db user |> Lwt_result.ok
    | None ->
      let language = context.Pool_tenant.Context.language in
      let open Sihl.Web in
      let csrf = HttpUtils.find_csrf req in
      let message = CCOption.bind (Flash.find_alert req) Message.of_string in
      Page.Public.login csrf language query_lang message ()
      |> Response.of_html
      |> Lwt.return_ok
  in
  result |> HttpUtils.extract_happy_path req
;;

let login_post req =
  let%lwt urlencoded = Sihl.Web.Request.to_urlencoded req in
  let result context =
    let open Lwt_result.Syntax in
    let query_lang = context.Pool_tenant.Context.query_language in
    Lwt_result.map_err (fun err ->
        err, HttpUtils.path_with_language query_lang "/login")
    @@
    let open Utils.Lwt_result.Infix in
    let* params =
      HttpUtils.urlencoded_to_params urlencoded [ "email"; "password" ]
      |> CCOption.to_result Pool_common.Message.LoginProvideDetails
      |> Lwt_result.lift
    in
    let tenant_db = context.Pool_tenant.Context.tenant_db in
    let email = List.assoc "email" params in
    let password = List.assoc "password" params in
    let* user =
      Service.User.login ~ctx:(to_ctx tenant_db) email ~password
      |> Lwt_result.map_err Pool_common.Message.handle_sihl_login_error
    in
    General.dashboard_path tenant_db query_lang user
    >|> CCFun.flip
          HttpUtils.redirect_to_with_actions
          [ Sihl.Web.Session.set [ "user_id", user.Sihl_user.id ] ]
    |> Lwt_result.ok
  in
  result |> HttpUtils.extract_happy_path req
;;

let request_reset_password_get req =
  let result context =
    let query_lang = context.Pool_tenant.Context.query_language in
    Lwt_result.map_err (fun err ->
        err, HttpUtils.path_with_language query_lang "/index")
    @@
    let open Utils.Lwt_result.Infix in
    let tenant_db = context.Pool_tenant.Context.tenant_db in
    let open Sihl.Web in
    Service.User.Web.user_from_session ~ctx:(to_ctx tenant_db) req
    >|> function
    | Some user ->
      General.dashboard_path tenant_db query_lang user
      ||> externalize_path
      ||> Response.redirect_to
      >|> Lwt.return_ok
    | None ->
      let csrf = HttpUtils.find_csrf req in
      let language = context.Pool_tenant.Context.language in
      let message = CCOption.bind (Flash.find_alert req) Message.of_string in
      Page.Public.request_reset_password csrf language query_lang message ()
      |> Response.of_html
      |> Lwt.return_ok
  in
  result |> HttpUtils.extract_happy_path req
;;

let request_reset_password_post req =
  let query_lang = HttpUtils.find_query_lang req in
  let%lwt result =
    let open Lwt_result.Syntax in
    let* context = Pool_tenant.Context.find req |> Lwt_result.lift in
    let open Utils.Lwt_result.Infix in
    let* email =
      Sihl.Web.Request.urlencoded "email" req
      ||> CCOption.to_result Pool_common.Message.(NotFound Email)
      >|= Pool_user.EmailAddress.of_string
    in
    let tenant_db = context.Pool_tenant.Context.tenant_db in
    let ctx = to_ctx tenant_db in
    let* participant = Participant.find_by_email tenant_db email in
    let language = context.Pool_tenant.Context.language in
    Email.Helper.PasswordReset.create
      tenant_db
      language
      ~user:participant.Participant.user
    >|= Service.Email.send ~ctx
  in
  match result with
  | Ok _ | Error _ ->
    HttpUtils.(
      redirect_to_with_actions
        (path_with_language query_lang "/request-reset-password")
        [ Message.set
            ~success:[ Pool_common.Message.PasswordResetSuccessMessage ]
        ])
;;

let reset_password_get req =
  let result context =
    let query_lang = context.Pool_tenant.Context.query_language in
    let error_path =
      "/request-reset-password/" |> HttpUtils.path_with_language query_lang
    in
    Lwt_result.map_err (fun err -> err, error_path)
    @@
    let token =
      Sihl.Web.Request.query Pool_common.Message.(field_name Token) req
    in
    match token with
    | None ->
      HttpUtils.redirect_to_with_actions
        error_path
        [ Message.set ~error:[ Pool_common.Message.(NotFound Token) ] ]
      |> Lwt_result.ok
    | Some token ->
      let csrf = HttpUtils.find_csrf req in
      let message =
        CCOption.bind (Sihl.Web.Flash.find_alert req) Message.of_string
      in
      let language = context.Pool_tenant.Context.language in
      Page.Public.reset_password csrf language query_lang message token ()
      |> Sihl.Web.Response.of_html
      |> Lwt_result.return
  in
  result |> HttpUtils.extract_happy_path req
;;

let reset_password_post req =
  let%lwt urlencoded = Sihl.Web.Request.to_urlencoded req in
  let result context =
    let open Lwt_result.Syntax in
    let query_lang = context.Pool_tenant.Context.query_language in
    let* params =
      HttpUtils.urlencoded_to_params
        urlencoded
        [ Pool_common.Message.(field_name Token)
        ; "password"
        ; "password_confirmation"
        ]
      |> CCOption.to_result
           ( Pool_common.Message.PasswordResetInvalidData
           , HttpUtils.path_with_language query_lang "/reset-password/" )
      |> Lwt_result.lift
    in
    let go = CCFun.flip List.assoc params in
    let token = go Pool_common.Message.(field_name Token) in
    let* () =
      Lwt_result.map_err (fun err ->
          ( err
          , query_lang
            |> CCOption.map (fun lang ->
                   [ ( Pool_common.Message.Language
                     , lang
                       |> Pool_common.Language.code
                       |> CCString.lowercase_ascii )
                   ])
            |> CCOption.value ~default:[]
            |> CCList.append [ Pool_common.Message.Token, token ]
            |> Pool_common.Message.add_field_query_params "/reset-password/" ))
      @@
      let tenant_db = context.Pool_tenant.Context.tenant_db in
      Service.PasswordReset.reset_password
        ~ctx:(to_ctx tenant_db)
        ~token
        (go "password")
        (go "password_confirmation")
      |> Lwt_result.map_err
           (CCFun.const Pool_common.Message.passwordresetinvaliddata)
    in
    HttpUtils.(
      redirect_to_with_actions
        (path_with_language query_lang "/login")
        [ Message.set ~success:[ Pool_common.Message.PasswordReset ] ])
    |> Lwt_result.ok
  in
  result |> HttpUtils.extract_happy_path req
;;

let logout req =
  let query_lang = Http_utils.find_query_lang req in
  HttpUtils.(
    redirect_to_with_actions
      (HttpUtils.path_with_language query_lang "/login")
      [ Sihl.Web.Session.set [ "user_id", "" ] ])
;;
