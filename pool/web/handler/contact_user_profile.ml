open Pool_message
module Command = Cqrs_command.Contact_command
module HttpUtils = Http_utils
module Conformist = Pool_common.Utils.PoolConformist
module User = Pool_user

let src = Logs.Src.create "handler.contact.user_profile"
let create_layout = Contact_general.create_layout
let tags = Pool_context.Logger.Tags.req
let contact_info_path = "/user/contact-information"

let show usage req =
  let result ({ Pool_context.database_label; language; user; _ } as context) =
    let open Utils.Lwt_result.Infix in
    Utils.Lwt_result.map_error (fun err -> err, "/login")
    @@ let* contact = Pool_context.find_contact context |> Lwt_result.lift in
       let create_layout active_navigation html =
         html
         |> create_layout ~active_navigation req context
         >|+ Sihl.Web.Response.of_html
       in
       match usage with
       | `ContactInformation ->
         let was_reset =
           let open CCOption in
           Sihl.Web.Request.query_list req
           |> CCList.assoc_opt ~eq:CCString.equal "reset"
           >>= CCList.head_opt
           |> CCOption.is_some
         in
         let%lwt verification =
           Contact.find_cell_phone_verification_by_contact
             database_label
             contact
         in
         Page.Contact.contact_information contact context verification was_reset
         |> create_layout contact_info_path
       | `LoginInformation ->
         let%lwt password_policy =
           I18n.find_by_key database_label I18n.Key.PasswordPolicyText language
         in
         Page.Contact.login_information contact context password_policy
         |> create_layout "/user/login-information"
       | `PersonalDetails ->
         let* tenant_languages =
           Pool_context.Tenant.find req
           |> Lwt_result.lift
           >|+ fun c -> c.Pool_context.Tenant.tenant_languages
         in
         let%lwt custom_fields =
           Custom_field.find_all_by_contact
             database_label
             user
             (Contact.id contact)
         in
         Page.Contact.personal_details
           contact
           custom_fields
           tenant_languages
           context
         |> create_layout "/user/personal-details"
  in
  result |> HttpUtils.extract_happy_path ~src req
;;

let personal_details = show `PersonalDetails
let login_information = show `LoginInformation
let contact_information = show `ContactInformation
let update = Helpers.PartialUpdate.update

let update_email req =
  let%lwt urlencoded = Sihl.Web.Request.to_urlencoded req in
  let result
    ({ Pool_context.database_label; query_language; language; user; _ } as
     context)
    =
    let open Utils.Lwt_result.Infix in
    let tags = tags req in
    Utils.Lwt_result.map_error (fun msg ->
      HttpUtils.(
        msg, "/user/login-information", [ urlencoded_to_flash urlencoded ]))
    @@ let* contact = Pool_context.find_contact context |> Lwt_result.lift in
       let%lwt allowed_email_suffixes =
         let open Utils.Lwt_result.Infix in
         Settings.find_email_suffixes database_label
         ||> fun suffixes ->
         if CCList.is_empty suffixes then None else Some suffixes
       in
       let* new_email =
         Pool_user.EmailAddress.create
           (CCList.assoc ~eq:CCString.equal Field.(Email |> show) urlencoded
            |> CCList.hd)
         |> Lwt_result.lift
       in
       let* () =
         let open Pool_context in
         let equal = Pool_user.EmailAddress.equal new_email in
         (fun is_equal ->
           if is_equal
           then Lwt_result.fail Error.EmailIdenticalToCurrent
           else Lwt_result.return ())
         @@
         match user with
         | Guest -> false
         | Admin admin -> Admin.email_address admin |> equal
         | Contact contact -> Contact.email_address contact |> equal
       in
       let%lwt existing_user =
         Service.User.find_by_email_opt
           ~ctx:(Pool_database.to_ctx database_label)
           (Pool_user.EmailAddress.value new_email)
       in
       let tenant = Pool_context.Tenant.get_tenant_exn req in
       let send_verification_mail unverified_contact =
         let* email_event =
           let%lwt token = Email.create_token database_label new_email in
           let%lwt verification_mail =
             let open Message_template in
             EmailVerification.create
               database_label
               language
               (Tenant tenant)
               contact
               new_email
               token
           in
           Command.RequestEmailValidation.(
             handle
               ~tags
               ?allowed_email_suffixes
               token
               verification_mail
               contact
               new_email
             |> Lwt_result.lift)
         in
         match unverified_contact with
         | None -> Lwt_result.return email_event
         | Some contact ->
           let* delete_event =
             Cqrs_command.Contact_command.DeleteUnverified.handle ~tags contact
             |> Lwt_result.lift
           in
           Lwt_result.return (email_event @ delete_event)
       in
       let* events =
         match existing_user with
         | None -> send_verification_mail None
         | Some user ->
           let change_attempt_notification () =
             Message_template.ContactEmailChangeAttempt.create tenant user
           in
           (match%lwt Admin.user_is_admin database_label user with
            | true ->
              let* notification = change_attempt_notification () in
              Lwt_result.return [ Email.Sent notification |> Pool_event.email ]
            | false ->
              let* contact = Contact.find_by_user database_label user in
              (match contact.Contact.email_verified with
               | Some _ ->
                 let* notification = change_attempt_notification () in
                 Lwt_result.return
                   [ Email.Sent notification |> Pool_event.email ]
               | None -> send_verification_mail (Some contact)))
       in
       let%lwt () = Pool_event.handle_events ~tags database_label events in
       HttpUtils.(
         redirect_to_with_actions
           (path_with_language query_language "/user/login-information")
           [ Message.set ~success:[ Success.EmailUpdateConfirmationMessage ] ])
       |> Lwt_result.ok
  in
  result |> HttpUtils.extract_happy_path_with_actions ~src req
;;

let update_password req =
  let%lwt urlencoded = Sihl.Web.Request.to_urlencoded req in
  let result
    ({ Pool_context.database_label; query_language; language; _ } as context)
    =
    let open Utils.Lwt_result.Infix in
    let tags = tags req in
    Utils.Lwt_result.map_error (fun msg ->
      HttpUtils.(
        msg, "/user/login-information", [ urlencoded_to_flash urlencoded ]))
    @@ let* contact = Pool_context.find_contact context |> Lwt_result.lift in
       let tenant = Pool_context.Tenant.get_tenant_exn req in
       let%lwt notification =
         Message_template.PasswordChange.create
           language
           tenant
           contact.Contact.user
       in
       let* events =
         let open CCResult.Infix in
         Command.UpdatePassword.(
           decode urlencoded >>= handle ~tags contact notification)
         |> Lwt_result.lift
       in
       let%lwt () = Pool_event.handle_events ~tags database_label events in
       HttpUtils.(
         redirect_to_with_actions
           (path_with_language query_language "/user/login-information")
           [ Message.set ~success:[ Success.PasswordChanged ] ])
       |> Lwt_result.ok
  in
  result |> HttpUtils.extract_happy_path_with_actions ~src req
;;

let update_cell_phone req =
  let%lwt urlencoded = Sihl.Web.Request.to_urlencoded req in
  let result
    ({ Pool_context.database_label; language; query_language; _ } as context)
    =
    let open Utils.Lwt_result.Infix in
    let tags = tags req in
    Utils.Lwt_result.map_error (fun msg ->
      HttpUtils.(msg, contact_info_path, [ urlencoded_to_flash urlencoded ]))
    @@ let* contact = Pool_context.find_contact context |> Lwt_result.lift in
       let* cell_phone =
         let find field =
           HttpUtils.find_in_urlencoded field urlencoded |> Lwt_result.lift
         in
         let* cell_phone = find Field.CellPhone in
         let* area_code = find Field.AreaCode in
         area_code
         |> CCInt.of_string
         |> (fun code ->
              CCOption.bind code Utils.PhoneCodes.find_code
              |> function
              | None -> Error (Error.Invalid Field.AreaCode)
              | Some (code, _) ->
                Format.asprintf "+%i%s" code cell_phone
                |> Pool_user.CellPhone.create)
         |> Lwt_result.lift
       in
       let token = Pool_common.VerificationCode.create () in
       let* () =
         let* { Pool_context.Tenant.tenant; _ } =
           Pool_context.Tenant.find req |> Lwt_result.lift
         in
         Message_template.PhoneVerification.create_text_message
           database_label
           language
           tenant
           contact
           cell_phone
           token
         |>> Text_message.Service.send database_label
       in
       let* events =
         Command.AddCellPhone.handle ~tags (contact, cell_phone, token)
         |> Lwt_result.lift
       in
       let%lwt () = Pool_event.handle_events ~tags database_label events in
       HttpUtils.(
         redirect_to_with_actions
           (path_with_language query_language contact_info_path)
           [ Message.set ~success:[ Success.CellPhoneTokenSent ] ])
       |> Lwt_result.ok
  in
  result |> HttpUtils.extract_happy_path_with_actions req
;;

let verify_cell_phone req =
  let%lwt urlencoded = Sihl.Web.Request.to_urlencoded req in
  let result ({ Pool_context.database_label; query_language; _ } as context) =
    let open Utils.Lwt_result.Infix in
    let tags = tags req in
    Utils.Lwt_result.map_error (fun msg ->
      HttpUtils.(msg, contact_info_path, [ urlencoded_to_flash urlencoded ]))
    @@ let* contact = Pool_context.find_contact context |> Lwt_result.lift in
       let* token =
         let open CCResult.Infix in
         HttpUtils.find_in_urlencoded Field.Token urlencoded
         >|= Pool_common.VerificationCode.of_string
         |> Lwt_result.lift
       in
       let* { User.UnverifiedCellPhone.cell_phone; _ } =
         Contact.find_cell_phone_verification_by_contact_and_code
           database_label
           contact
           token
       in
       let* events =
         Command.VerifyCellPhone.handle ~tags (contact, cell_phone)
         |> Lwt_result.lift
       in
       let%lwt () = Pool_event.handle_events ~tags database_label events in
       HttpUtils.(
         redirect_to_with_actions
           (path_with_language query_language contact_info_path)
           [ Message.set ~success:[ Success.CellPhoneVerified ] ])
       |> Lwt_result.ok
  in
  result |> HttpUtils.extract_happy_path_with_actions req
;;

let reset_phone_verification req =
  let result ({ Pool_context.database_label; query_language; _ } as context) =
    let open Utils.Lwt_result.Infix in
    let tags = tags req in
    Utils.Lwt_result.map_error (fun msg -> msg, contact_info_path, [])
    @@ let* contact = Pool_context.find_contact context |> Lwt_result.lift in
       let* events =
         Command.ResetCellPhoneVerification.handle ~tags contact
         |> Lwt_result.lift
       in
       let%lwt () = Pool_event.handle_events ~tags database_label events in
       HttpUtils.(
         redirect_to
           (path_with_language
              query_language
              (Format.asprintf "%s?reset=true" contact_info_path)))
       |> Lwt_result.ok
  in
  result |> HttpUtils.extract_happy_path_with_actions req
;;

let resend_token req =
  let result
    ({ Pool_context.database_label; language; query_language; _ } as context)
    =
    let open Utils.Lwt_result.Infix in
    Utils.Lwt_result.map_error (fun msg -> msg, contact_info_path, [])
    @@ let* contact = Pool_context.find_contact context |> Lwt_result.lift in
       let* { Pool_context.Tenant.tenant; _ } =
         Pool_context.Tenant.find req |> Lwt_result.lift
       in
       let* { Pool_user.UnverifiedCellPhone.cell_phone; verification_code; _ } =
         Contact.find_full_cell_phone_verification_by_contact
           database_label
           contact
       in
       let* () =
         Message_template.PhoneVerification.create_text_message
           database_label
           language
           tenant
           contact
           cell_phone
           verification_code
         |>> Text_message.Service.send database_label
       in
       HttpUtils.(
         redirect_to_with_actions
           (path_with_language query_language contact_info_path)
           [ Message.set ~success:[ Success.VerificationMessageResent ] ])
       |> Lwt_result.ok
  in
  result |> HttpUtils.extract_happy_path_with_actions req
;;

let completion req =
  let open Utils.Lwt_result.Infix in
  let result ({ Pool_context.database_label; user; _ } as context) =
    Utils.Lwt_result.map_error (fun err -> err, "/login")
    @@
    let flash_fetcher key = Sihl.Web.Flash.find key req in
    let* contact = Pool_context.find_contact context |> Lwt_result.lift in
    let%lwt custom_fields =
      Custom_field.find_unanswered_required_by_contact
        database_label
        user
        (Contact.id contact)
    in
    Page.Contact.completion context flash_fetcher custom_fields
    |> create_layout req ~active_navigation:"/user" context
    >|+ Sihl.Web.Response.of_html
  in
  result |> HttpUtils.extract_happy_path ~src req
;;

let completion_post req =
  let open Utils.Lwt_result.Infix in
  let%lwt urlencoded =
    Sihl.Web.Request.to_urlencoded req
    ||> HttpUtils.format_request_boolean_values []
    ||> HttpUtils.remove_empty_values
  in
  let result
    ({ Pool_context.database_label; query_language; language; user; _ } as
     context)
    =
    Utils.Lwt_result.map_error (fun err ->
      HttpUtils.(
        ( err
        , path_with_language query_language "/user/completion"
        , [ urlencoded_to_flash urlencoded ] )))
    @@
    let tags = tags req in
    let* contact = Pool_context.find_contact context |> Lwt_result.lift in
    let contact_id = Contact.id contact in
    let%lwt custom_fields =
      Custom_field.find_unanswered_ungrouped_required_by_contact
        database_label
        user
        contact_id
    in
    let events =
      let open Utils.Lwt_result.Infix in
      let handle =
        Cqrs_command.Custom_field_answer_command.UpdateMultiple.handle
          ~tags
          user
          contact_id
      in
      Helpers_custom_field.answer_and_validate_multiple
        req
        urlencoded
        language
        (Contact.Id.to_common contact_id)
        custom_fields
      >== fun fields -> fields |> CCList.map handle |> CCList.all_ok
    in
    let handle events =
      let%lwt (_ : unit list) =
        Lwt_list.map_s (Pool_event.handle_event ~tags database_label) events
      in
      let%lwt required_answers_given =
        Custom_field.all_required_answered database_label (Contact.id contact)
      in
      match required_answers_given with
      | true ->
        Http_utils.(
          redirect_to_with_actions
            "/experiments"
            [ Message.set ~success:[ Success.Updated Field.Profile ] ])
        ||> Sihl.Web.Session.set_value
              ~key:Contact.profile_completion_cookie
              ""
              req
      | false ->
        Http_utils.(
          redirect_to_with_actions
            "/user/completion"
            [ Message.set ~error:[ Error.RequiredFieldsMissing ] ])
    in
    events |>> handle
  in
  result |> HttpUtils.extract_happy_path_with_actions ~src req
;;

let pause_account req =
  let open Utils.Lwt_result.Infix in
  let redirect_path = "/user/personal-details" in
  let result context =
    Page.Contact.pause_account context ()
    |> create_layout req context
    >|+ Sihl.Web.Response.of_html
    >|- fun err -> err, redirect_path
  in
  result |> HttpUtils.extract_happy_path ~src req
;;

let toggle_paused req =
  let open Utils.Lwt_result.Infix in
  let redirect_path = "/user/personal-details" in
  let tags = tags req in
  let result context =
    let* contact =
      Pool_context.find_contact context
      |> Lwt_result.lift
      >|- fun err -> err, redirect_path
    in
    Helpers.ContactUpdate.toggle_paused context redirect_path contact tags
  in
  result |> HttpUtils.extract_happy_path ~src req
;;
