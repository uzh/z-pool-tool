module HttpUtils = Http_utils
module Message = HttpUtils.Message

let src = Logs.Src.create "handler.public.login"
let to_ctx = Pool_database.to_ctx
let create_layout req = General.create_tenant_layout req

let increase_sign_in_count ~tags database_label =
  let open Utils.Lwt_result.Infix in
  function
  | `Admin _ -> Lwt_result.return ()
  | `Contact contact ->
    Cqrs_command.Contact_command.UpdateSignInCount.handle ~tags contact
    |> Lwt_result.lift
    |>> Pool_event.handle_events database_label
;;

let login_get req =
  let open Utils.Lwt_result.Infix in
  let flash_fetcher = CCFun.flip Sihl.Web.Flash.find req in
  let result context =
    Utils.Lwt_result.map_error (fun err -> err, "/index")
    @@
    let open Sihl.Web in
    Page.Public.login
      ?intended:(HttpUtils.find_intended_opt req)
      context
      flash_fetcher
    |> create_layout req ~active_navigation:"/login" context
    >|+ Response.of_html
  in
  result |> HttpUtils.extract_happy_path ~src req
;;

let login_post req =
  let%lwt urlencoded = Sihl.Web.Request.to_urlencoded req in
  let to_sihl_user = function
    | `Admin user -> user
    | `Contact contact -> contact.Contact.user
  in
  let result { Pool_context.database_label; query_language; _ } =
    let open Utils.Lwt_result.Infix in
    Utils.Lwt_result.map_error (fun err ->
      ( err
      , "/login" |> HttpUtils.intended_of_request req
      , [ HttpUtils.urlencoded_to_flash urlencoded ] ))
    @@ let* user = Helpers.Login.login req urlencoded database_label in
       let login user ?(set_completion_cookie = false) path actions =
         let sihl_user = to_sihl_user user in
         let tags = Pool_context.Logger.Tags.req req in
         let* () = increase_sign_in_count ~tags database_label user in
         HttpUtils.(
           redirect_to_with_actions
             (path_with_language query_language path)
             ([ Sihl.Web.Session.set [ "user_id", sihl_user.Sihl_user.id ] ]
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
       let success user () =
         let open Pool_context in
         let%lwt path =
           match HttpUtils.find_intended_opt req with
           | Some intended -> Lwt.return intended
           | None ->
             user
             |> to_sihl_user
             |> user_of_sihl_user database_label
             ||> Pool_context.dashboard_path
         in
         login user path []
       in
       let contact user =
         Contact.find
           database_label
           (user.Sihl_user.id |> Pool_common.Id.of_string)
       in
       user
       |> Admin.user_is_admin database_label
       >|> function
       | true -> success (`Admin user) ()
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
             | true -> success (`Contact contact) ()
             | false ->
               login
                 (`Contact contact)
                 ~set_completion_cookie:true
                 "/user/completion"
                 [ Message.set
                     ~error:[ Pool_common.Message.(RequiredFieldsMissing) ]
                 ]))
  in
  result |> HttpUtils.extract_happy_path_with_actions ~src req
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
  result |> HttpUtils.extract_happy_path ~src req
;;

let request_reset_password_post req =
  let open HttpUtils in
  let open Cqrs_command.Common_command.ResetPassword in
  let open Message_template in
  let result { Pool_context.database_label; query_language; language; _ } =
    let redirect_path =
      path_with_language query_language "/request-reset-password"
    in
    Lwt_result.map_error (fun err ->
      err, redirect_path, [ (fun res -> Message.set ~error:[ err ] res) ])
    @@
    let tags = Pool_context.Logger.Tags.req req in
    let open Utils.Lwt_result.Infix in
    let tenant = Pool_context.Tenant.get_tenant_exn req in
    let ctx = Pool_database.to_ctx database_label in
    let* user =
      Sihl.Web.Request.to_urlencoded req
      ||> decode
      >|+ Pool_user.EmailAddress.value
      |>> Service.User.find_by_email_opt ~ctx
    in
    let* () =
      match user with
      | None -> Lwt_result.return ()
      | Some user ->
        PasswordReset.create database_label language (Tenant tenant) user
        >== handle ~tags
        |>> Pool_event.handle_events ~tags database_label
    in
    redirect_to_with_actions
      redirect_path
      [ Message.set ~success:[ Pool_common.Message.PasswordResetSuccessMessage ]
      ]
    >|> Lwt_result.return
  in
  result |> extract_happy_path_with_actions ~src req
;;

let reset_password_get req =
  let result ({ Pool_context.database_label; language; _ } as context) =
    let open Utils.Lwt_result.Infix in
    let error_path = "/request-reset-password/" in
    Utils.Lwt_result.map_error (fun err -> err, error_path)
    @@
    let token =
      Sihl.Web.Request.query Pool_common.Message.Field.(Token |> show) req
    in
    let%lwt password_policy =
      I18n.find_by_key database_label I18n.Key.PasswordPolicyText language
    in
    match token with
    | None ->
      HttpUtils.redirect_to_with_actions
        error_path
        [ Message.set ~error:[ Pool_common.Message.(NotFound Field.Token) ] ]
      |> Lwt_result.ok
    | Some token ->
      Page.Public.reset_password token context password_policy
      |> create_layout req ~active_navigation:"/reset-password" context
      >|+ Sihl.Web.Response.of_html
  in
  result |> HttpUtils.extract_happy_path ~src req
;;

let reset_password_post req =
  let%lwt urlencoded = Sihl.Web.Request.to_urlencoded req in
  let result { Pool_context.database_label; query_language; _ } =
    let open Utils.Lwt_result.Infix in
    let open Pool_common.Message in
    let redirect = "/reset-password/" in
    let* params =
      Field.[ Token; Password; PasswordConfirmation ]
      |> CCList.map Field.show
      |> HttpUtils.urlencoded_to_params urlencoded
      |> CCOption.to_result (PasswordResetInvalidData, redirect)
      |> Lwt_result.lift
    in
    let go field = field |> Field.show |> CCFun.flip List.assoc params in
    let token = go Field.Token in
    let redirect_with_param =
      add_field_query_params redirect [ Field.Token, token ]
    in
    let* (_ : Pool_user.Password.t) =
      let open Pool_user.Password in
      Field.Password
      |> go
      |> create
      |> Lwt_result.lift
      >|- fun err -> err, redirect_with_param
    in
    let%lwt reset =
      Service.PasswordReset.reset_password
        ~ctx:(to_ctx database_label)
        ~token
        (go Field.Password)
        (go Field.PasswordConfirmation)
      >|- CCFun.const (passwordresetinvaliddata, redirect_with_param)
    in
    match reset with
    | Ok () ->
      let%lwt () =
        Service.Token.deactivate
          ~ctx:(Pool_database.to_ctx database_label)
          token
      in
      HttpUtils.(
        redirect_to_with_actions
          (path_with_language query_language "/login")
          [ Message.set ~success:[ PasswordReset ] ])
      |> Lwt_result.ok
    | Error err -> err |> Lwt_result.fail
  in
  result |> HttpUtils.extract_happy_path ~src req
;;

let logout req =
  let query_lang = Http_utils.find_query_lang req in
  HttpUtils.(
    redirect_to_with_actions
      (HttpUtils.path_with_language query_lang "/login")
      [ Sihl.Web.Session.set [ "user_id", "" ] ])
;;
