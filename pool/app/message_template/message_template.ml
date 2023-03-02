include Entity
include Event
include Default
include Message_utils
module Guard = Entity_guard

let find = Repo.find
let all_default = Repo.all_default
let find_all_of_entity_by_label = Repo.find_all_of_entity_by_label

let find_all_by_label_and_languages ?entity_uuids pool languages label =
  let open Utils.Lwt_result.Infix in
  Lwt_list.map_s
    (fun lang ->
      Repo.find_by_label ?entity_uuids pool lang label >|+ CCPair.make lang)
    languages
  ||> CCResult.flatten_l
;;

let filter_languages languages templates =
  languages
  |> CCList.filter (fun lang ->
       CCList.find_opt
         (fun template -> Pool_common.Language.equal template.language lang)
         templates
       |> CCOption.is_none)
;;

let find_available_languages database_label entity_id label languages =
  let%lwt existing =
    find_all_of_entity_by_label database_label entity_id label
  in
  filter_languages languages existing |> Lwt.return
;;

let prepare_email language template sender email layout params =
  let { Entity.email_subject; email_text; plain_text; _ } = template in
  let mail =
    Sihl_email.
      { sender = Settings.ContactEmail.value sender
      ; recipient = Pool_user.EmailAddress.value email
      ; subject = email_subject
      ; text = PlainText.value plain_text
      ; html = Some (combine_html language (Some email_subject))
      ; cc = []
      ; bcc = []
      }
  in
  let params = [ "emailText", email_text ] @ layout_params layout @ params in
  Sihl_email.Template.render_email_with_data params mail
;;

let global_params user = [ "name", user |> Pool_user.user_fullname ]

let experiment_params experiment =
  let open Experiment in
  [ "experimentPublicTitle", public_title_value experiment
  ; "experimentDescription", description_value experiment
  ]
;;

module AssignmentConfirmation = struct
  let base_params contact = contact.Contact.user |> global_params

  let email_params lang session contact =
    base_params contact
    @ [ "sessionOverview", Session.to_email_text lang session ]
  ;;

  let email_params_public_session lang session contact =
    base_params contact
    @ [ "sessionOverview", Session.public_to_email_text lang session ]
  ;;

  let template pool language =
    Repo.find_by_label pool language Label.AssignmentConfirmation
  ;;

  let create pool language tenant session contact =
    let open Utils.Lwt_result.Infix in
    let* template = template pool language in
    let params = email_params language session contact in
    let layout = layout_from_tenant tenant in
    let email = contact |> Contact.email_address in
    let%lwt sender = Pool_tenant.Service.Email.sender_of_pool pool in
    prepare_email language template sender email layout params
    |> Lwt_result.return
  ;;

  let create_from_public_session pool language tenant session contact =
    let open Utils.Lwt_result.Infix in
    let* template = template pool language in
    let params = email_params_public_session language session contact in
    let layout = layout_from_tenant tenant in
    let email = contact |> Contact.email_address in
    let%lwt sender = Pool_tenant.Service.Email.sender_of_pool pool in
    prepare_email language template sender email layout params
    |> Lwt_result.return
  ;;
end

module ContactRegistrationAttempt = struct
  let email_params tenant_url contact =
    let reset_url = create_public_url tenant_url "/request-reset-password" in
    global_params contact.Contact.user
    @ [ "tenantUrl", Pool_tenant.Url.value tenant_url
      ; "resetUrl", reset_url
      ; ( "emailAddress"
        , Pool_user.EmailAddress.value (Contact.email_address contact) )
      ]
  ;;

  let create pool language tenant contact =
    let open Message_utils in
    let open Utils.Lwt_result.Infix in
    let* template =
      Repo.find_by_label pool language Label.ContactRegistrationAttempt
    in
    let layout = layout_from_tenant tenant in
    let tenant_url = tenant.Pool_tenant.url in
    let%lwt sender = Pool_tenant.Service.Email.sender_of_pool pool in
    prepare_email
      language
      template
      sender
      (contact |> Contact.email_address)
      layout
      (email_params tenant_url contact)
    |> Lwt_result.return
  ;;
end

module EmailVerification = struct
  let email_params validation_url contact =
    global_params contact.Contact.user @ [ "verificationUrl", validation_url ]
  ;;

  let create pool language layout contact email_address token =
    let open Message_utils in
    let open Utils.Lwt_result.Infix in
    let* template = Repo.find_by_label pool language Label.EmailVerification in
    let%lwt url = Pool_tenant.Url.of_pool pool in
    let validation_url =
      Pool_common.
        [ ( Message.Field.Language
          , language |> Language.show |> CCString.lowercase_ascii )
        ; Message.Field.Token, Email.Token.value token
        ]
      |> create_public_url_with_params url "/email-verified"
    in
    let%lwt sender = Pool_tenant.Service.Email.sender_of_pool pool in
    prepare_email
      language
      template
      sender
      email_address
      (create_layout layout)
      (email_params validation_url contact)
    |> Lwt_result.return
  ;;
end

module ExperimentInvitation = struct
  let email_params experiment public_url contact =
    let open Experiment in
    let id = experiment.Experiment.id |> Id.value in
    global_params contact.Contact.user
    @ [ ( "experimentUrl"
        , create_public_url public_url (Format.asprintf "experiments/%s" id) )
      ]
    @ experiment_params experiment
  ;;

  let prepare tenant experiment =
    let open Message_utils in
    let open Utils.Lwt_result.Infix in
    let pool = tenant.Pool_tenant.database_label in
    let%lwt sys_langs = Settings.find_languages pool in
    let* templates =
      find_all_by_label_and_languages
        ~entity_uuids:[ Experiment.(Id.to_common experiment.Experiment.id) ]
        pool
        sys_langs
        Label.ExperimentInvitation
    in
    let%lwt tenant_url = Pool_tenant.Url.of_pool pool in
    let%lwt sender =
      Pool_tenant.Service.Email.sender_of_pool tenant.Pool_tenant.database_label
    in
    let layout = layout_from_tenant tenant in
    let fnc (contact : Contact.t) =
      let open CCResult in
      let* lang, template = template_by_contact sys_langs templates contact in
      let params = email_params experiment tenant_url contact in
      prepare_email
        lang
        template
        sender
        (Contact.email_address contact)
        layout
        params
      |> CCResult.return
    in
    Lwt_result.return fnc
  ;;

  let create ({ Pool_tenant.database_label; _ } as tenant) experiment contact =
    let open Message_utils in
    let open Utils.Lwt_result.Infix in
    let%lwt system_languages = Settings.find_languages database_label in
    let* language =
      message_langauge system_languages contact |> Lwt_result.lift
    in
    let* template =
      Repo.find_by_label database_label language Label.ExperimentInvitation
    in
    let%lwt tenant_url = Pool_tenant.Url.of_pool database_label in
    let%lwt sender = Pool_tenant.Service.Email.sender_of_pool database_label in
    let layout = layout_from_tenant tenant in
    let params = email_params experiment tenant_url contact in
    prepare_email
      language
      template
      sender
      (Contact.email_address contact)
      layout
      params
    |> Lwt_result.return
  ;;
end

module PasswordChange = struct
  let email_params user = global_params user

  let create pool language tenant user =
    let open Utils.Lwt_result.Infix in
    let* template = Repo.find_by_label pool language Label.PasswordChange in
    let layout = layout_from_tenant tenant in
    let email = Pool_user.user_email_address user in
    let%lwt sender = Pool_tenant.Service.Email.sender_of_pool pool in
    prepare_email language template sender email layout (email_params user)
    |> Lwt_result.return
  ;;
end

module PasswordReset = struct
  let email_params reset_url user =
    (user |> global_params) @ [ "resetUrl", reset_url ]
  ;;

  let create pool language layout user =
    let open Message_utils in
    let open Utils.Lwt_result.Infix in
    let email = Pool_user.user_email_address user in
    let* template = Repo.find_by_label pool language Label.PasswordReset in
    let%lwt url = Pool_tenant.Url.of_pool pool in
    let%lwt sender = Pool_tenant.Service.Email.sender_of_pool pool in
    let open Pool_common in
    let* reset_token =
      Service.PasswordReset.create_reset_token
        ~ctx:(Pool_tenant.to_ctx pool)
        (Pool_user.EmailAddress.value email)
      ||> function
      | None ->
        Logs.err (fun m -> m "Reset token not found");
        Error Message.PasswordResetFailMessage
      | Some token -> Ok token
    in
    let reset_url =
      Message.
        [ Field.Token, reset_token
        ; Field.Language, language |> Language.show |> CCString.lowercase_ascii
        ]
      |> create_public_url_with_params
           url
           (prepend_root_directory pool "/reset-password/")
    in
    prepare_email
      language
      template
      sender
      email
      (create_layout layout)
      (email_params reset_url user)
    |> Lwt_result.return
  ;;
end

module ProfileUpdateTrigger = struct
  let email_params tenant_url contact =
    let profile_url = create_public_url tenant_url "/user/personal-details" in
    (contact.Contact.user |> global_params) @ [ "profileUrl", profile_url ]
  ;;

  let prepare pool tenant =
    let open Message_utils in
    let open Utils.Lwt_result.Infix in
    let%lwt sys_langs = Settings.find_languages pool in
    let* templates =
      find_all_by_label_and_languages pool sys_langs Label.SessionReschedule
    in
    let%lwt url = Pool_tenant.Url.of_pool pool in
    let%lwt sender = Pool_tenant.Service.Email.sender_of_pool pool in
    let fnc contact =
      let open CCResult in
      let* lang, template = template_by_contact sys_langs templates contact in
      prepare_email
        lang
        template
        sender
        (Contact.email_address contact)
        (layout_from_tenant tenant)
        (email_params url contact)
      |> CCResult.return
    in
    Lwt_result.return fnc
  ;;
end

module SessionCancellation = struct
  let email_params lang session reason contact =
    global_params contact.Contact.user
    @ [ "sessionOverview", Session.to_email_text lang session
      ; "reason", reason |> Session.CancellationReason.value
      ]
  ;;

  let prepare pool tenant sys_langs session =
    let open Message_utils in
    let open Utils.Lwt_result.Infix in
    let* templates =
      find_all_by_label_and_languages pool sys_langs Label.SessionCancellation
    in
    let%lwt sender = Pool_tenant.Service.Email.sender_of_pool pool in
    let layout = layout_from_tenant tenant in
    let fnc reason (contact : Contact.t) =
      let open CCResult in
      let* lang, template = template_by_contact sys_langs templates contact in
      let params = email_params lang session reason contact in
      prepare_email
        lang
        template
        sender
        (Contact.email_address contact)
        layout
        params
      |> CCResult.return
    in
    Lwt_result.return fnc
  ;;
end

module SessionReminder = struct
  let email_params lang experiment session contact =
    global_params contact.Contact.user
    @ (("sessionOverview", Session.to_email_text lang session)
      :: experiment_params experiment)
  ;;

  let create pool tenant system_languages experiment session contact =
    let open Message_utils in
    let open Utils.Lwt_result.Infix in
    let* language =
      message_langauge system_languages contact |> Lwt_result.lift
    in
    let* template =
      Repo.find_by_label
        ~entity_uuids:
          [ session.Session.id
          ; Experiment.(Id.to_common experiment.Experiment.id)
          ]
        pool
        language
        Label.SessionReminder
    in
    let%lwt sender = Pool_tenant.Service.Email.sender_of_pool pool in
    let layout = layout_from_tenant tenant in
    let params = email_params language experiment session contact in
    prepare_email
      language
      template
      sender
      (Contact.email_address contact)
      layout
      params
    |> Lwt_result.return
  ;;

  let prepare pool tenant sys_langs experiment session =
    let open Message_utils in
    let open Utils.Lwt_result.Infix in
    let* templates =
      find_all_by_label_and_languages
        ~entity_uuids:
          [ session.Session.id
          ; Experiment.(Id.to_common experiment.Experiment.id)
          ]
        pool
        sys_langs
        Label.SessionReminder
    in
    let%lwt sender = Pool_tenant.Service.Email.sender_of_pool pool in
    let layout = layout_from_tenant tenant in
    let fnc contact =
      let open CCResult in
      let* lang, template = template_by_contact sys_langs templates contact in
      let params = email_params lang experiment session contact in
      prepare_email
        lang
        template
        sender
        (Contact.email_address contact)
        layout
        params
      |> CCResult.return
    in
    Lwt_result.return fnc
  ;;
end

module SessionReschedule = struct
  let email_params lang session new_start new_duration contact =
    let open Pool_common.Utils.Time in
    let open Session in
    global_params contact.Contact.user
    @ [ "sessionOverview", to_email_text lang session
      ; "newStart", new_start |> Start.value |> formatted_date_time
      ; "newDuration", new_duration |> Duration.value |> formatted_timespan
      ]
  ;;

  let prepare pool tenant sys_langs session =
    let open Message_utils in
    let open Utils.Lwt_result.Infix in
    let* templates =
      find_all_by_label_and_languages pool sys_langs Label.SessionReschedule
    in
    let%lwt sender = Pool_tenant.Service.Email.sender_of_pool pool in
    let layout = layout_from_tenant tenant in
    let fnc (contact : Contact.t) new_start new_duration =
      let open CCResult in
      let* lang, template = template_by_contact sys_langs templates contact in
      let params = email_params lang session new_start new_duration contact in
      prepare_email
        lang
        template
        sender
        (Contact.email_address contact)
        layout
        params
      |> CCResult.return
    in
    Lwt_result.return fnc
  ;;
end

module SignUpVerification = struct
  let email_params verification_url firstname lastname =
    let open Pool_user in
    [ ( "name"
      , Format.asprintf
          "%s %s"
          (Firstname.value firstname)
          (Lastname.value lastname) )
    ; "verificationUrl", verification_url
    ]
  ;;

  let create pool language tenant email_address token firstname lastname =
    let open Message_utils in
    let open Utils.Lwt_result.Infix in
    let* template = Repo.find_by_label pool language Label.SignUpVerification in
    let%lwt url = Pool_tenant.Url.of_pool pool in
    let%lwt sender = Pool_tenant.Service.Email.sender_of_pool pool in
    let verification_url =
      Pool_common.
        [ ( Message.Field.Language
          , language |> Language.show |> CCString.lowercase_ascii )
        ; Message.Field.Token, Email.Token.value token
        ]
      |> create_public_url_with_params url "/email-verified"
    in
    prepare_email
      language
      template
      sender
      email_address
      (layout_from_tenant tenant)
      (email_params verification_url firstname lastname)
    |> Lwt_result.return
  ;;
end
