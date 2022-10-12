module HttpUtils = Http_utils
module Message = HttpUtils.Message

let to_ctx = Pool_tenant.to_ctx
let create_layout req = General.create_tenant_layout `Contact req

let redirect_to_dashboard tenant_db user =
  let open Lwt.Infix in
  General.dashboard_path tenant_db user >>= HttpUtils.redirect_to
;;

let login_get req =
  let open Lwt_result.Infix in
  let result ({ Pool_context.tenant_db; _ } as context) =
    Lwt_result.map_error (fun err -> err, "/index")
    @@ let%lwt user =
         Service.User.Web.user_from_session ~ctx:(to_ctx tenant_db) req
       in
       match user with
       | Some user -> redirect_to_dashboard tenant_db user |> Lwt_result.ok
       | None ->
         let open Sihl.Web in
         Page.Public.login context
         |> create_layout req ~active_navigation:"/login" context
         >|= Response.of_html
  in
  result |> HttpUtils.extract_happy_path req
;;

let login_post req =
  let%lwt urlencoded = Sihl.Web.Request.to_urlencoded req in
  let result { Pool_context.tenant_db; query_language; _ } =
    let open Lwt_result.Syntax in
    let open Utils.Lwt_result.Infix in
    let open Pool_common.Message in
    Lwt_result.map_error (fun err -> err, "/login")
    @@ let* params =
         Field.[ Email; Password ]
         |> CCList.map Field.show
         |> HttpUtils.urlencoded_to_params urlencoded
         |> CCOption.to_result LoginProvideDetails
         |> Lwt_result.lift
       in
       let email = List.assoc Field.(Email |> show) params in
       let password = List.assoc Field.(Password |> show) params in
       let* user =
         Service.User.login ~ctx:(to_ctx tenant_db) email ~password
         |> Lwt_result.map_error handle_sihl_login_error
       in
       let redirect path actions =
         HttpUtils.(
           redirect_to_with_actions
             (path_with_language query_language path)
             ([ Sihl.Web.Session.set [ "user_id", user.Sihl_user.id ] ]
             @ actions))
         |> Lwt_result.ok
       in
       let success () =
         let%lwt path = General.dashboard_path tenant_db user in
         redirect path []
       in
       let contact user =
         Contact.find tenant_db (user.Sihl_user.id |> Pool_common.Id.of_string)
       in
       user
       |> Admin.user_is_admin tenant_db
       >|> function
       | true -> success ()
       | false ->
         let* contact = user |> contact in
         let%lwt required_answers_given =
           Custom_field.all_required_answered tenant_db (Contact.id contact)
         in
         (match required_answers_given with
          | true -> success ()
          | false ->
            redirect
              "/user/completion"
              [ Message.set
                  ~info:[ Pool_common.Message.(RequiredFieldsMissing) ]
              ])
  in
  result |> HttpUtils.extract_happy_path req
;;

let request_reset_password_get req =
  let result ({ Pool_context.tenant_db; _ } as context) =
    Lwt_result.map_error (fun err -> err, "/index")
    @@
    let open Utils.Lwt_result.Infix in
    let open Sihl.Web in
    Service.User.Web.user_from_session ~ctx:(to_ctx tenant_db) req
    >|> function
    | Some user ->
      General.dashboard_path tenant_db user
      ||> externalize_path
      ||> Response.redirect_to
      >|> Lwt.return_ok
    | None ->
      Page.Public.request_reset_password context
      |> create_layout req ~active_navigation:"/request-reset-password" context
      >|= Response.of_html
  in
  result |> HttpUtils.extract_happy_path req
;;

let request_reset_password_post req =
  let open HttpUtils in
  let open Cqrs_command.Common_command.ResetPassword in
  let query_lang = find_query_lang req in
  let result { Pool_context.tenant_db; language; _ } =
    let open Utils.Lwt_result.Infix in
    Sihl.Web.Request.to_urlencoded req
    ||> decode
    >>= Contact.find_by_email tenant_db
    >== (fun { Contact.user; _ } -> handle user language)
    |>> Pool_event.handle_events tenant_db
    >|> function
    | Ok () | Error (_ : Pool_common.Message.error) ->
      redirect_to_with_actions
        (path_with_language query_lang "/request-reset-password")
        [ Message.set
            ~success:[ Pool_common.Message.PasswordResetSuccessMessage ]
        ]
      >|> Lwt.return_ok
  in
  result |> extract_happy_path_with_actions req
;;

let reset_password_get req =
  let result context =
    let open Lwt_result.Infix in
    let error_path = "/request-reset-password/" in
    Lwt_result.map_error (fun err -> err, error_path)
    @@
    let token =
      Sihl.Web.Request.query Pool_common.Message.Field.(Token |> show) req
    in
    match token with
    | None ->
      HttpUtils.redirect_to_with_actions
        error_path
        [ Message.set ~error:[ Pool_common.Message.(NotFound Field.Token) ] ]
      |> Lwt_result.ok
    | Some token ->
      Page.Public.reset_password token context
      |> create_layout req ~active_navigation:"/reset-password" context
      >|= Sihl.Web.Response.of_html
  in
  result |> HttpUtils.extract_happy_path req
;;

let reset_password_post req =
  let%lwt urlencoded = Sihl.Web.Request.to_urlencoded req in
  let result { Pool_context.tenant_db; query_language; _ } =
    let open Lwt_result.Syntax in
    let open Pool_common.Message in
    let* params =
      Field.[ Token; Password; PasswordConfirmation ]
      |> CCList.map Field.show
      |> HttpUtils.urlencoded_to_params urlencoded
      |> CCOption.to_result (PasswordResetInvalidData, "/reset-password/")
      |> Lwt_result.lift
    in
    let go field = field |> Field.show |> CCFun.flip List.assoc params in
    let token = go Field.Token in
    let* () =
      Service.PasswordReset.reset_password
        ~ctx:(to_ctx tenant_db)
        ~token
        (go Field.Password)
        (go Field.PasswordConfirmation)
      |> Lwt_result.map_error
           (CCFun.const
              ( passwordresetinvaliddata
              , [ Field.Token, token ]
                |> add_field_query_params "/reset-password/" ))
    in
    HttpUtils.(
      redirect_to_with_actions
        (path_with_language query_language "/login")
        [ Message.set ~success:[ PasswordReset ] ])
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
