open CCFun.Infix
module HttpUtils = Http_utils
module Message = HttpUtils.Message

let src = Logs.Src.create "handler.public.login"
let to_ctx = Database.to_ctx
let create_layout req = General.create_tenant_layout req

let increase_sign_in_count ~tags database_label user =
  let open Utils.Lwt_result.Infix in
  let open Pool_context in
  let open Cqrs_command in
  let events =
    match user with
    | Admin admin -> Admin_command.UpdateSignInCount.handle ~tags admin
    | Contact contact -> Contact_command.UpdateSignInCount.handle ~tags contact
    | Guest -> Ok []
  in
  events |> Lwt_result.lift |>> Pool_event.handle_events database_label
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
  let to_user =
    let open Pool_context in
    function
    | Admin { Admin.user; _ } -> Ok user
    | Contact { Contact.user; _ } -> Ok user
    | Guest -> Error Pool_message.(Error.NotFound Field.User)
  in
  let result { Pool_context.database_label; query_parameters; _ } =
    let open Utils.Lwt_result.Infix in
    Utils.Lwt_result.map_error (fun err ->
      ( err
      , "/login" |> HttpUtils.intended_of_request req
      , [ HttpUtils.urlencoded_to_flash urlencoded ] ))
    @@ let* user = Helpers.Login.login req urlencoded database_label in
       let login user ?(set_completion_cookie = false) path actions =
         let* pool_user = to_user user |> Lwt_result.lift in
         let tags = Pool_context.Logger.Tags.req req in
         let* () = increase_sign_in_count ~tags database_label user in
         HttpUtils.(
           redirect_to_with_actions
             (url_with_field_params query_parameters path)
             ([ Sihl.Web.Session.set
                  [ "user_id", pool_user.Pool_user.id |> Pool_user.Id.value ]
              ]
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
         HttpUtils.(redirect_to (url_with_field_params query_parameters path))
         |> Lwt_result.ok
       in
       let success user () =
         let path =
           match HttpUtils.find_intended_opt req with
           | Some intended -> intended
           | None -> user |> Pool_context.dashboard_path
         in
         login user path []
       in
       let find_contact user =
         user.Pool_user.id |> Contact.(Id.of_user %> find database_label)
       in
       let find_admin user =
         user.Pool_user.id
         |> Admin.(Id.of_user %> find database_label)
         >|+ Pool_context.admin
       in
       match user |> Pool_user.is_confirmed with
       | false ->
         redirect
           (Http_utils.url_with_field_params
              query_parameters
              "/email-confirmation")
       | true ->
         user
         |> Admin.user_is_admin database_label
         >|> (function
          | true ->
            let* admin = find_admin user in
            success admin ()
          | false ->
            let* contact = user |> find_contact in
            let%lwt required_answers_given =
              Custom_field.all_required_answered
                database_label
                (Contact.id contact)
            in
            let contact = contact |> Pool_context.contact in
            (match required_answers_given with
             | true -> success contact ()
             | false ->
               login
                 contact
                 ~set_completion_cookie:true
                 "/user/completion"
                 [ Message.set
                     ~error:[ Pool_message.Error.RequiredFieldsMissing ]
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
  let result ({ Pool_context.database_label; query_parameters; _ } as context) =
    let redirect_path =
      url_with_field_params query_parameters "/request-reset-password"
    in
    Lwt_result.map_error (fun err ->
      err, redirect_path, [ (fun res -> Message.set ~error:[ err ] res) ])
    @@
    let tags = Pool_context.Logger.Tags.req req in
    let open Utils.Lwt_result.Infix in
    let tenant = Pool_context.Tenant.get_tenant_exn req in
    let tenant_languages = Pool_context.Tenant.get_tenant_languages_exn req in
    let* user =
      Sihl.Web.Request.to_urlencoded req
      ||> decode
      |>> Pool_user.find_active_by_email_opt database_label
    in
    let* () =
      match user with
      | None -> Lwt_result.return ()
      | Some user ->
        let%lwt message_language =
          match
            Pool_context.Utils.query_language tenant_languages query_parameters
          with
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
        PasswordReset.create
          database_label
          message_language
          (Tenant tenant)
          user
        >== handle ~tags
        |>> Pool_event.handle_events ~tags database_label
    in
    redirect_to_with_actions
      redirect_path
      [ Message.set
          ~success:[ Pool_message.Success.PasswordResetSuccessMessage ]
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
    let token = Sihl.Web.Request.query Pool_message.Field.(Token |> show) req in
    let%lwt password_policy =
      I18n.find_by_key database_label I18n.Key.PasswordPolicyText language
    in
    match token with
    | None ->
      HttpUtils.redirect_to_with_actions
        error_path
        [ Message.set ~error:[ Pool_message.(Error.NotFound Field.Token) ] ]
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
  let result { Pool_context.database_label; query_parameters; _ } =
    let open Utils.Lwt_result.Infix in
    let open Pool_message in
    let redirect = "/reset-password/" in
    let tags = Pool_context.Logger.Tags.req req in
    let* params =
      Field.[ Token; Password; PasswordConfirmation ]
      |> CCList.map Field.show
      |> HttpUtils.urlencoded_to_params urlencoded
      |> CCOption.to_result (Error.PasswordResetInvalidData, redirect)
      |> Lwt_result.lift
    in
    let go field =
      field |> Field.show |> CCFun.flip (CCList.assoc ~eq:( = )) params
    in
    let token = go Field.Token in
    let redirect_with_param =
      add_field_query_params redirect [ Field.Token, token ]
    in
    let password = Field.Password |> go |> Pool_user.Password.Plain.create in
    let password_confirmed =
      let open Pool_user.Password.Confirmation in
      Field.PasswordConfirmation |> go |> create
    in
    let* user_uuid =
      Pool_token.read database_label token ~k:"user_id"
      ||> CCOption.to_result Pool_message.(Error.Invalid Field.Token)
      >|- (fun err -> err, redirect)
      >|+ Pool_user.Id.of_string
    in
    let%lwt reset =
      Pool_user.Password.Reset.reset_password
        database_label
        ~token
        password
        password_confirmed
      >|- CCFun.const (Error.PasswordResetInvalidData, redirect_with_param)
    in
    let* import_events =
      Lwt_result.map_error (fun err -> err, redirect)
      @@
      let%lwt import =
        User_import.find_pending_by_user_id_opt database_label user_uuid
      in
      import
      |> function
      | None -> Lwt_result.return []
      | Some import ->
        let%lwt user =
          Pool_user.find_exn database_label user_uuid
          >|> Pool_context.context_user_of_user database_label
        in
        Cqrs_command.User_import_command.DisableImport.handle
          ~tags
          (user, import)
        |> Lwt_result.lift
    in
    match reset with
    | Ok () ->
      let%lwt () = Pool_token.deactivate database_label token in
      let%lwt () = import_events |> Pool_event.handle_events database_label in
      HttpUtils.(
        redirect_to_with_actions
          (url_with_field_params query_parameters "/login")
          [ Message.set ~success:[ Success.PasswordReset ] ])
      |> Lwt_result.ok
    | Error err -> err |> Lwt_result.fail
  in
  result |> HttpUtils.extract_happy_path ~src req
;;

let logout req =
  HttpUtils.(
    redirect_to_with_actions
      (HttpUtils.retain_url_params req "/login" |> Uri.to_string)
      [ Sihl.Web.Session.set [ "user_id", "" ] ])
;;
