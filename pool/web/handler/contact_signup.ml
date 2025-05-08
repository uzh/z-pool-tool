open Pool_message
module Command = Cqrs_command.Contact_command
module UserCommand = Cqrs_command.User_command
module HttpUtils = Http_utils
module Response = Http_response

let src = Logs.Src.create "handler.contact.signup"
let create_layout = Contact_general.create_layout

let sign_up req =
  let result ({ Pool_context.database_label; language; _ } as context) =
    let open Utils.Lwt_result.Infix in
    Response.bad_request_render_error context
    @@
    let%lwt custom_fields = Custom_field.all_prompted_on_registration database_label in
    let%lwt terms =
      I18n.find_by_key database_label I18n.Key.TermsAndConditions language
    in
    Page.Contact.sign_up terms custom_fields context
    |> create_layout req ~active_navigation:"/signup" context
    >|+ Sihl.Web.Response.of_html
  in
  Response.handle ~src req result
;;

let sign_up_create req =
  let open Utils.Lwt_result.Infix in
  let terms_key = Field.(TermsAccepted |> show) in
  let user_id = Contact.Id.create () in
  let%lwt urlencoded =
    Sihl.Web.Request.to_urlencoded req
    ||> HttpUtils.remove_empty_values
    ||> HttpUtils.format_request_boolean_values [ terms_key ]
  in
  let result { Pool_context.database_label; query_parameters; language; user; _ } =
    let open Utils.Lwt_result.Infix in
    let tags = Pool_context.Logger.Tags.req req in
    Response.bad_request_on_error ~urlencoded sign_up
    @@ let* () = Helpers.terms_and_conditions_accepted urlencoded in
       let%lwt allowed_email_suffixes =
         let open Utils.Lwt_result.Infix in
         Settings.find_email_suffixes database_label
         ||> fun suffixes -> if CCList.is_empty suffixes then None else Some suffixes
       in
       let* answered_custom_fields =
         Custom_field.all_prompted_on_registration database_label
         >|> Helpers_custom_field.answer_and_validate_multiple
               req
               urlencoded
               language
               (user_id |> Contact.Id.to_common)
       in
       let tenant = Pool_context.Tenant.get_tenant_exn req in
       let tenant_languages = Pool_context.Tenant.get_tenant_languages_exn req in
       let* email_address =
         Sihl.Web.Request.urlencoded Field.(Email |> show) req
         ||> CCOption.to_result Error.ContactSignupInvalidEmail
         >== Pool_user.EmailAddress.create
       in
       let log_request () =
         Logging_helper.log_request_with_ip
           ~src
           "User registration"
           req
           tags
           (Some email_address)
       in
       let create_contact_events () =
         let* ({ UserCommand.firstname; lastname; _ } as decoded) =
           Command.SignUp.decode urlencoded |> Lwt_result.lift
         in
         let%lwt token = Email.create_token database_label email_address in
         let signup_code =
           let open Signup_code in
           let open CCOption.Infix in
           Pool_context.Utils.find_query_param query_parameters url_key
           >>= CCFun.(Code.create %> CCResult.to_opt)
         in
         let query_language =
           Pool_context.Utils.query_language tenant_languages query_parameters
         in
         let%lwt verification_mail =
           Message_template.SignUpVerification.create
             ?signup_code
             database_label
             (CCOption.value ~default:language query_language)
             tenant
             email_address
             token
             firstname
             lastname
             (Contact.Id.to_user user_id)
         in
         decoded
         |> Command.SignUp.handle
              ~tags
              ?allowed_email_suffixes
              ~user_id
              ?signup_code
              answered_custom_fields
              token
              email_address
              verification_mail
              query_language
         |> Lwt_result.lift
       in
       let%lwt existing_user = Pool_user.find_by_email_opt database_label email_address in
       let* events =
         match existing_user with
         | None ->
           let* events = create_contact_events () in
           log_request ();
           Lwt_result.return events
         | Some user when Pool_user.is_admin user -> Lwt_result.return []
         | Some _ ->
           let%lwt contact = email_address |> Contact.find_by_email database_label in
           let* events =
             contact
             |> function
             | Ok contact when contact |> Contact.user |> Pool_user.is_confirmed ->
               let%lwt send_notification =
                 Contact.should_send_registration_attempt_notification
                   database_label
                   contact
               in
               if not send_notification
               then Lwt_result.return []
               else
                 contact
                 |> Contact.user
                 |> Message_template.ContactRegistrationAttempt.create
                      (CCOption.value ~default:language contact.Contact.language)
                      tenant
                 ||> Command.SendRegistrationAttemptNotifitacion.handle ~tags contact
             | Ok contact ->
               let* create_contact_events = create_contact_events () in
               let open CCResult.Infix in
               contact
               |> Command.DeleteUnverified.handle ~tags
               >|= CCFun.flip CCList.append create_contact_events
               |> Lwt_result.lift
             | Error _ -> Lwt_result.return []
           in
           log_request ();
           Lwt_result.return events
       in
       let%lwt () = Pool_event.handle_events ~tags database_label user events in
       HttpUtils.(
         redirect_to_with_actions
           "/email-confirmation"
           [ Message.set ~success:[ Success.EmailConfirmationMessage ] ])
       |> Lwt_result.ok
  in
  Response.handle ~src req result
;;

let email_verification req =
  let open Utils.Lwt_result.Infix in
  let tags = Pool_context.Logger.Tags.req req in
  let result ({ Pool_context.database_label; query_parameters; user; _ } as context) =
    (* TODO: Is this endpoint only used for unverified users? Or also to update user email?

       * We probably need a redirect response to redirect if the user is logged in
    *)
    Response.bad_request_on_error sign_up
    @@
    let%lwt redirect_path =
      let user =
        Pool_context.find_contact context
        |> CCResult.map (fun contact -> contact.Contact.user)
        |> CCOption.of_result
      in
      match user with
      | None -> "/login" |> Lwt.return
      | Some user ->
        let open Pool_context in
        context_user_of_user database_label user ||> dashboard_path
    in
    let* token =
      Sihl.Web.Request.query Field.(show Token) req
      |> CCOption.map Email.Token.create
      |> CCOption.to_result (Error.NotFound Field.Token)
      |> Lwt_result.lift
    in
    let* email =
      Pool_token.read database_label (Email.Token.value token) ~k:Field.(Email |> show)
      ||> CCOption.to_result Error.TokenInvalidFormat
      >== Pool_user.EmailAddress.create
      >>= Email.find_unverified_by_address database_label
      |> Lwt_result.map_error (fun _ -> Error.Invalid Field.Token)
    in
    let* events =
      let open UserCommand in
      let signup_code =
        let open Signup_code in
        let open CCOption.Infix in
        Pool_context.Utils.find_query_param query_parameters url_key
        >>= CCFun.(Code.create %> CCResult.to_opt)
      in
      let%lwt admin =
        Admin.find database_label (email |> Email.user_id |> Admin.Id.of_user)
      in
      let%lwt contact =
        Contact.find database_label (Email.user_id email |> Contact.Id.of_user)
      in
      let verify_email ?signup_code user =
        VerifyEmail.(handle ~tags ?signup_code user email) |> Lwt_result.lift
      in
      let update_email user = UpdateEmail.(handle ~tags user email) |> Lwt_result.lift in
      match email |> Email.user_is_confirmed, contact, admin with
      | false, Ok contact, _ -> verify_email ?signup_code (Contact contact)
      | true, Ok contact, _ -> update_email (Contact contact)
      | false, Error _, Ok admin -> verify_email (Admin admin)
      | true, _, Ok admin -> update_email (Admin admin)
      | true, Error _, Error _ | false, Error _, Error _ ->
        Logs.err (fun m ->
          m
            ~tags
            "Impossible email update tried: %s with context: %s"
            ([%show: Email.t] email)
            ([%show: Pool_context.t] context));
        Lwt.return_ok []
    in
    let%lwt () = Pool_event.handle_events ~tags database_label user events in
    HttpUtils.(
      redirect_to_with_actions
        (url_with_field_params query_parameters redirect_path)
        [ Message.set ~success:[ Success.EmailVerified ] ])
    |> Lwt_result.ok
  in
  Response.handle ~src req result
;;

(* TODO: Check, if this calls terms_accept *)
let terms req =
  let open Utils.Lwt_result.Infix in
  let result ({ Pool_context.database_label; language; query_parameters; _ } as context) =
    Response.bad_request_render_error context
    @@
    let%lwt terms =
      I18n.find_by_key database_label I18n.Key.TermsAndConditions language
    in
    let notification =
      Pool_context.Utils.find_query_param query_parameters Field.Redirected
      |> CCOption.map (CCFun.const Pool_common.I18n.TermsAndConditionsUpdated)
    in
    Page.Contact.terms ?notification terms context
    |> create_layout req context
    >|+ Sihl.Web.Response.of_html
  in
  Response.handle ~src req result
;;

let terms_accept req =
  let result ({ Pool_context.database_label; query_parameters; user; _ } as context) =
    Response.bad_request_on_error terms
    @@
    let open Utils.Lwt_result.Infix in
    let tags = Pool_context.Logger.Tags.req req in
    let* contact = Pool_context.find_contact context |> Lwt_result.lift in
    let* events =
      Command.AcceptTermsAndConditions.handle ~tags contact |> Lwt_result.lift
    in
    let%lwt () = Pool_event.handle_events ~tags database_label user events in
    HttpUtils.(redirect_to (url_with_field_params query_parameters "/experiments"))
    |> Lwt_result.ok
  in
  Response.handle ~src req result
;;
