include Entity
include Event
include Default
include Message_utils
module Guard = Entity_guard

let src = Logs.Src.create "message_template"
let find = Repo.find
let all_default = Repo.all_default
let find_all_of_entity_by_label = Repo.find_all_of_entity_by_label
let find_by_label_to_send = Repo.find_by_label_to_send
let find_all_by_label_to_send = Repo.find_all_by_label_to_send
let sender_of_pool = Email.Service.sender_of_pool

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

module AccountSuspensionNotification = struct
  let email_params = global_params

  let create ({ Pool_tenant.database_label; _ } as tenant) user =
    let open Message_utils in
    let open Utils.Lwt_result.Infix in
    let open Sihl.Contract in
    let%lwt system_languages = Settings.find_languages database_label in
    let email = user.User.email |> Pool_user.EmailAddress.of_string in
    let* preferred_langauge =
      match user.User.admin with
      | true -> Lwt_result.return Pool_common.Language.En
      | false ->
        email
        |> Contact.find_by_email database_label
        >|+ preferred_language system_languages
    in
    let%lwt template, language =
      find_by_label_to_send
        database_label
        preferred_langauge
        Label.AccountSuspensionNotification
    in
    let%lwt sender = sender_of_pool database_label in
    let layout = layout_from_tenant tenant in
    let params = email_params user in
    prepare_email language template sender email layout params
    |> Lwt_result.return
  ;;
end

module AssignmentConfirmation = struct
  let base_params contact = contact.Contact.user |> global_params

  let email_params lang sessions contact =
    let session_overview =
      sessions
      |> CCList.map (Session.to_email_text lang)
      |> CCString.concat "\n\n"
    in
    base_params contact @ [ "sessionOverview", session_overview ]
  ;;

  let email_params_public_session lang sessions contact =
    let session_overview =
      sessions
      |> CCList.map (Session.public_to_email_text lang)
      |> CCString.concat "\n\n"
    in
    base_params contact @ [ "sessionOverview", session_overview ]
  ;;

  let template pool language =
    find_by_label_to_send pool language Label.AssignmentConfirmation
  ;;

  let create pool preferred_language tenant sessions contact =
    let%lwt template, language = template pool preferred_language in
    let params = email_params language sessions contact in
    let layout = layout_from_tenant tenant in
    let email = contact |> Contact.email_address in
    let%lwt sender = sender_of_pool pool in
    prepare_email language template sender email layout params |> Lwt.return
  ;;

  let create_from_public_session pool preferred_language tenant sessions contact
    =
    let%lwt template, language = template pool preferred_language in
    let params = email_params_public_session language sessions contact in
    let layout = layout_from_tenant tenant in
    let email = contact |> Contact.email_address in
    let%lwt sender = sender_of_pool pool in
    prepare_email language template sender email layout params |> Lwt.return
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

  let create pool preferred_language tenant contact =
    let%lwt template, language =
      find_by_label_to_send
        pool
        preferred_language
        Label.ContactRegistrationAttempt
    in
    let layout = layout_from_tenant tenant in
    let tenant_url = tenant.Pool_tenant.url in
    let%lwt sender = sender_of_pool pool in
    prepare_email
      language
      template
      sender
      (contact |> Contact.email_address)
      layout
      (email_params tenant_url contact)
    |> Lwt.return
  ;;
end

module EmailVerification = struct
  let email_params validation_url contact =
    global_params contact.Contact.user @ [ "verificationUrl", validation_url ]
  ;;

  let create pool preferred_language layout contact email_address token =
    let%lwt template, language =
      find_by_label_to_send pool preferred_language Label.EmailVerification
    in
    let%lwt url = Pool_tenant.Url.of_pool pool in
    let validation_url =
      Pool_common.
        [ ( Message.Field.Language
          , language |> Language.show |> CCString.lowercase_ascii )
        ; Message.Field.Token, Email.Token.value token
        ]
      |> create_public_url_with_params url "/email-verified"
    in
    let%lwt sender = sender_of_pool pool in
    prepare_email
      language
      template
      sender
      email_address
      (create_layout layout)
      (email_params validation_url contact)
    |> Lwt.return
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
    let pool = tenant.Pool_tenant.database_label in
    let%lwt sys_langs = Settings.find_languages pool in
    let%lwt templates =
      find_all_by_label_to_send
        pool
        ~entity_uuids:[ Experiment.(Id.to_common experiment.Experiment.id) ]
        sys_langs
        Label.ExperimentInvitation
    in
    let%lwt tenant_url = Pool_tenant.Url.of_pool pool in
    let%lwt sender = sender_of_pool tenant.Pool_tenant.database_label in
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
    Lwt.return fnc
  ;;

  let create ({ Pool_tenant.database_label; _ } as tenant) experiment contact =
    let open Message_utils in
    let%lwt system_languages = Settings.find_languages database_label in
    let preferred_langauge = preferred_language system_languages contact in
    let%lwt template, language =
      find_by_label_to_send
        database_label
        preferred_langauge
        Label.ExperimentInvitation
    in
    let%lwt tenant_url = Pool_tenant.Url.of_pool database_label in
    let%lwt sender = sender_of_pool database_label in
    let layout = layout_from_tenant tenant in
    let params = email_params experiment tenant_url contact in
    prepare_email
      language
      template
      sender
      (Contact.email_address contact)
      layout
      params
    |> Lwt.return
  ;;
end

module PasswordChange = struct
  let email_params user = global_params user

  let create pool preferred_langauge tenant user =
    let%lwt template, language =
      find_by_label_to_send pool preferred_langauge Label.PasswordChange
    in
    let layout = layout_from_tenant tenant in
    let email = Pool_user.user_email_address user in
    let%lwt sender = sender_of_pool pool in
    prepare_email language template sender email layout (email_params user)
    |> Lwt.return
  ;;
end

module PasswordReset = struct
  let email_params reset_url user =
    (user |> global_params) @ [ "resetUrl", reset_url ]
  ;;

  let create pool preferred_language layout user =
    let open Utils.Lwt_result.Infix in
    let email = Pool_user.user_email_address user in
    let%lwt template, language =
      find_by_label_to_send pool preferred_language Label.PasswordReset
    in
    let%lwt url = Pool_tenant.Url.of_pool pool in
    let%lwt sender = sender_of_pool pool in
    let open Pool_common in
    let* reset_token =
      Service.PasswordReset.create_reset_token
        ~ctx:(Pool_database.to_ctx pool)
        (Pool_user.EmailAddress.value email)
      ||> function
      | None ->
        Logs.err ~src (fun m ->
          m
            ~tags:(Pool_database.Logger.Tags.create pool)
            "Reset token not found");
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

module PhoneVerification = struct
  let message_params token =
    [ "token", Pool_common.VerificationCode.value token ]
  ;;

  let create_text_message
    pool
    preferred_language
    (tenant : Pool_tenant.t)
    phone_number
    token
    =
    let open Text_message in
    let%lwt { sms_text; _ }, _ =
      find_by_label_to_send pool preferred_language Label.PhoneVerification
    in
    let content = Content.render sms_text (message_params token) in
    Lwt_result.return
      { recipient = phone_number
      ; sender = tenant.Pool_tenant.title
      ; text = content
      }
  ;;
end

module ProfileUpdateTrigger = struct
  let email_params tenant_url contact =
    let profile_url = create_public_url tenant_url "/user/personal-details" in
    (contact.Contact.user |> global_params) @ [ "profileUrl", profile_url ]
  ;;

  let prepare pool tenant =
    let open Message_utils in
    let%lwt sys_langs = Settings.find_languages pool in
    let%lwt templates =
      find_all_by_label_to_send pool sys_langs Label.SessionReschedule
    in
    let%lwt url = Pool_tenant.Url.of_pool pool in
    let%lwt sender = sender_of_pool pool in
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
    Lwt.return fnc
  ;;
end

module SessionCancellation = struct
  let email_params
    lang
    (tenant : Pool_tenant.t)
    (experiment : Experiment.t)
    session
    follow_up_sessions
    reason
    contact
    =
    let experiment_url =
      let open Experiment in
      experiment.id
      |> Id.value
      |> Format.asprintf "/experiments/%s"
      |> create_public_url tenant.Pool_tenant.url
    in
    let main_session = Session.to_email_text lang session in
    let session_overview =
      match follow_up_sessions with
      | [] -> main_session
      | sessions ->
        let follow_ups =
          [ Pool_common.(
              Utils.hint_to_string lang I18n.SessionCancellationMessageFollowUps)
          ; Session.follow_up_sessions_to_email_list sessions
          ]
          |> CCString.concat "\n"
        in
        [ main_session; follow_ups ] |> CCString.concat "\n\n"
    in
    global_params contact.Contact.user
    @ [ "experimentUrl", experiment_url
      ; "reason", reason |> Session.CancellationReason.value
      ; "sessionOverview", session_overview
      ]
  ;;

  let prepare pool tenant experiment sys_langs session follow_up_sessions =
    let open Message_utils in
    let%lwt templates =
      find_all_by_label_to_send pool sys_langs Label.SessionCancellation
    in
    let%lwt sender = sender_of_pool pool in
    let layout = layout_from_tenant tenant in
    let fnc reason (contact : Contact.t) =
      let open CCResult in
      let* lang, template = template_by_contact sys_langs templates contact in
      let params =
        email_params
          lang
          tenant
          experiment
          session
          follow_up_sessions
          reason
          contact
      in
      prepare_email
        lang
        template
        sender
        (Contact.email_address contact)
        layout
        params
      |> CCResult.return
    in
    Lwt.return fnc
  ;;

  let prepare_text_message
    pool
    (tenant : Pool_tenant.t)
    experiment
    sys_langs
    session
    follow_up_sessions
    =
    let open Message_utils in
    let%lwt templates =
      find_all_by_label_to_send pool sys_langs Label.SessionCancellation
    in
    let title = tenant.Pool_tenant.title in
    let fnc reason (contact : Contact.t) phone_number =
      let open CCResult in
      let* lang, template = template_by_contact sys_langs templates contact in
      let params =
        email_params
          lang
          tenant
          experiment
          session
          follow_up_sessions
          reason
          contact
      in
      Text_message.render_and_create
        phone_number
        title
        (template.sms_text, params)
      |> CCResult.return
    in
    Lwt.return fnc
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
    let preferred_language = preferred_language system_languages contact in
    let%lwt template, language =
      find_by_label_to_send
        ~entity_uuids:
          [ Session.Id.to_common session.Session.id
          ; Experiment.Id.to_common experiment.Experiment.id
          ]
        pool
        preferred_language
        Label.SessionReminder
    in
    let%lwt sender = sender_of_pool pool in
    let layout = layout_from_tenant tenant in
    let params = email_params language experiment session contact in
    prepare_email
      language
      template
      sender
      (Contact.email_address contact)
      layout
      params
    |> Lwt.return
  ;;

  let prepare pool tenant sys_langs experiment session =
    let open Message_utils in
    let%lwt templates =
      find_all_by_label_to_send
        ~entity_uuids:
          [ Session.Id.to_common session.Session.id
          ; Experiment.Id.to_common experiment.Experiment.id
          ]
        pool
        sys_langs
        Label.SessionReminder
    in
    let%lwt sender = sender_of_pool pool in
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
    Lwt.return fnc
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
    let%lwt templates =
      find_all_by_label_to_send pool sys_langs Label.SessionReschedule
    in
    let%lwt sender = sender_of_pool pool in
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
    Lwt.return fnc
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

  let create
    pool
    preferred_language
    tenant
    email_address
    token
    firstname
    lastname
    =
    let%lwt template, language =
      find_by_label_to_send pool preferred_language Label.SignUpVerification
    in
    let%lwt url = Pool_tenant.Url.of_pool pool in
    let%lwt sender = sender_of_pool pool in
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
    |> Lwt.return
  ;;
end
