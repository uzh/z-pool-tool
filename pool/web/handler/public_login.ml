module HttpUtils = Http_utils
module Message = HttpUtils.Message

let to_ctx = Pool_tenant.to_ctx
let create_layout req = General.create_tenant_layout req

let redirect_to_dashboard user =
  Pool_context.dashboard_path user |> HttpUtils.redirect_to
;;

let login_get req =
  let open Utils.Lwt_result.Infix in
  let result context =
    Utils.Lwt_result.map_error (fun err -> err, "/index")
    @@
    let open Sihl.Web in
    Page.Public.login context
    |> create_layout req ~active_navigation:"/login" context
    >|+ Response.of_html
  in
  result |> HttpUtils.extract_happy_path req
;;

let login_post req =
  let%lwt urlencoded = Sihl.Web.Request.to_urlencoded req in
  let result { Pool_context.database_label; query_language; _ } =
    let open Utils.Lwt_result.Infix in
    let open Pool_common.Message in
    Utils.Lwt_result.map_error (fun err -> err, "/login")
    @@ let* params =
         Field.[ Email; Password ]
         |> CCList.map Field.show
         |> HttpUtils.urlencoded_to_params urlencoded
         |> CCOption.to_result LoginProvideDetails
         |> Lwt_result.lift
       in
       let email = CCList.assoc ~eq:String.equal Field.(Email |> show) params in
       let password =
         CCList.assoc ~eq:String.equal Field.(Password |> show) params
       in
       let* user =
         Service.User.login ~ctx:(to_ctx database_label) email ~password
         >|- handle_sihl_login_error
       in
       let login ?(set_completion_cookie = false) path actions =
         HttpUtils.(
           redirect_to_with_actions
             (path_with_language query_language path)
             ([ Sihl.Web.Session.set [ "user_id", user.Sihl_user.id ] ]
             @ actions))
         ||> (fun res ->
               if set_completion_cookie
               then
                 Sihl.Web.Session.set_value
                   ~key:Contact.profile_completion_cookie
                   "true"
                   req
                   res
               else res)
         |> Lwt_result.ok
       in
       let redirect path =
         HttpUtils.(redirect_to (path_with_language query_language path))
         |> Lwt_result.ok
       in
       let success () =
         let open Pool_context in
         let%lwt path =
           user |> user_of_sihl_user database_label ||> dashboard_path
         in
         login path []
       in
       let contact user =
         Contact.find
           database_label
           (user.Sihl_user.id |> Pool_common.Id.of_string)
       in
       user
       |> Admin.user_is_admin database_label
       >|> function
       | true -> success ()
       | false ->
         (match user.Sihl_user.confirmed with
          | false ->
            redirect
              (Http_utils.path_with_language
                 query_language
                 "/email-confirmation")
          | true ->
            let* contact = user |> contact in
            let%lwt required_answers_given =
              Custom_field.all_required_answered
                database_label
                (Contact.id contact)
            in
            (match required_answers_given with
             | true -> success ()
             | false ->
               login
                 ~set_completion_cookie:true
                 "/user/completion"
                 [ Message.set
                     ~error:[ Pool_common.Message.(RequiredFieldsMissing) ]
                 ]))
  in
  result |> HttpUtils.extract_happy_path req
;;

let request_reset_password_get req =
  let result context =
    Utils.Lwt_result.map_error (fun err -> err, "/index")
    @@
    let open Utils.Lwt_result.Infix in
    let open Sihl.Web in
    Page.Public.request_reset_password context
    |> create_layout req ~active_navigation:"/request-reset-password" context
    >|+ Response.of_html
  in
  result |> HttpUtils.extract_happy_path req
;;

let request_reset_password_post req =
  let open HttpUtils in
  let open Cqrs_command.Common_command.ResetPassword in
  let result { Pool_context.database_label; query_language; language; _ } =
    let redirect_path =
      path_with_language query_language "/request-reset-password"
    in
    let tags = Logger.req req in
    let open Utils.Lwt_result.Infix in
    let open Lwt_result.Syntax in
    let* { Pool_context.Tenant.tenant; _ } =
      Pool_context.Tenant.find req
      |> CCResult.map_err (fun err ->
           err, redirect_path, [ (fun res -> Message.set ~error:[ err ] res) ])
      |> Lwt_result.lift
    in
    let layout = Email.Helper.layout_from_tenant tenant in
    Sihl.Web.Request.to_urlencoded req
    ||> decode
    >>= Contact.find_by_email database_label
    >== (fun { Contact.user; _ } -> handle layout language user)
    |>> Pool_event.handle_events ~tags database_label
    >|> function
    | Ok () | Error (_ : Pool_common.Message.error) ->
      redirect_to_with_actions
        redirect_path
        [ Message.set
            ~success:[ Pool_common.Message.PasswordResetSuccessMessage ]
        ]
      >|> Lwt.return_ok
  in
  result |> extract_happy_path_with_actions req
;;

let reset_password_get req =
  let result context =
    let open Utils.Lwt_result.Infix in
    let error_path = "/request-reset-password/" in
    Utils.Lwt_result.map_error (fun err -> err, error_path)
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
      >|+ Sihl.Web.Response.of_html
  in
  result |> HttpUtils.extract_happy_path req
;;

let reset_password_post req =
  let%lwt urlencoded = Sihl.Web.Request.to_urlencoded req in
  let result { Pool_context.database_label; query_language; _ } =
    let open Utils.Lwt_result.Infix in
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
        ~ctx:(to_ctx database_label)
        ~token
        (go Field.Password)
        (go Field.PasswordConfirmation)
      >|- CCFun.const
            ( passwordresetinvaliddata
            , [ Field.Token, token ]
              |> add_field_query_params "/reset-password/" )
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
