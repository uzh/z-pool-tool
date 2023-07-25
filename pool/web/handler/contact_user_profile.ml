module Command = Cqrs_command.Contact_command
module HttpUtils = Http_utils
module Conformist = Pool_common.Utils.PoolConformist
module User = Pool_user
module PoolField = Pool_common.Message.Field

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
  let open Pool_common.Message in
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
       let%lwt token = Email.create_token database_label new_email in
       let tenant = Pool_context.Tenant.get_tenant_exn req in
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
       let* events =
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
       Utils.Database.with_transaction database_label (fun () ->
         let%lwt () = Pool_event.handle_events ~tags database_label events in
         HttpUtils.(
           redirect_to_with_actions
             (path_with_language query_language "/email-confirmation")
             [ Message.set ~success:[ EmailConfirmationMessage ] ]))
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
           database_label
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
       Utils.Database.with_transaction database_label (fun () ->
         let%lwt () = Pool_event.handle_events ~tags database_label events in
         HttpUtils.(
           redirect_to_with_actions
             (path_with_language query_language "/user/login-information")
             [ Message.set ~success:[ Pool_common.Message.PasswordChanged ] ]))
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
         let* cell_phone = find PoolField.CellPhone in
         let* area_code = find PoolField.AreaCode in
         area_code
         |> CCInt.of_string
         |> (fun code ->
              CCOption.bind code Utils.PhoneCodes.find_code
              |> function
              | None -> Error Pool_common.Message.(Invalid Field.AreaCode)
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
           [ Message.set ~success:[ Pool_common.Message.CellPhoneTokenSent ] ])
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
         HttpUtils.find_in_urlencoded PoolField.Token urlencoded
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
           [ Message.set ~success:[ Pool_common.Message.CellPhoneVerified ] ])
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
           cell_phone
           verification_code
         |>> Text_message.Service.send database_label
       in
       HttpUtils.(
         redirect_to_with_actions
           (path_with_language query_language contact_info_path)
           [ Message.set
               ~success:[ Pool_common.Message.VerificationMessageResent ]
           ])
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
    let open Custom_field in
    let tags = tags req in
    let* contact = Pool_context.find_contact context |> Lwt_result.lift in
    let%lwt custom_fields =
      urlencoded
      |> CCList.map (fun pair -> pair |> fst |> Pool_common.Id.of_string)
      |> Custom_field.find_multiple_by_contact
           database_label
           (Contact.id contact)
    in
    let events =
      let open Utils.Lwt_result.Infix in
      let open Public in
      let handle =
        Cqrs_command.Custom_field_answer_command.UpdateMultiple.handle
          ~tags
          user
          (Contact.id contact)
      in
      Lwt_list.map_s
        (fun field ->
          let id = field |> Public.id |> Id.value in
          (match field with
           | MultiSelect _ ->
             req
             |> HttpUtils.htmx_urlencoded_list
                  (field
                   |> Public.to_common_field language
                   |> Pool_common.Message.Field.array_key)
           | Boolean _ | Date _ | Number _ | Select _ | Text _ ->
             CCList.assoc_opt ~eq:CCString.equal id urlencoded
             |> CCOption.value ~default:[]
             |> Lwt.return)
          ||> CCFun.flip (Custom_field.validate_htmx ~is_admin:false) field
          >>= fun field -> handle field |> Lwt_result.lift)
        custom_fields
      ||> CCList.all_ok
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
            [ Message.set
                ~success:[ Pool_common.Message.(Updated Field.Profile) ]
            ])
        ||> Sihl.Web.Session.set_value
              ~key:Contact.profile_completion_cookie
              ""
              req
      | false ->
        Http_utils.(
          redirect_to_with_actions
            "/user/completion"
            [ Message.set ~error:[ Pool_common.Message.(RequiredFieldsMissing) ]
            ])
    in
    events |>> handle
  in
  result |> HttpUtils.extract_happy_path_with_actions ~src req
;;
