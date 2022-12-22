include Entity
include Event
include Default
open Message_utils

let prepare_email language template email layout params =
  match Sihl.Configuration.read_string "SMTP_SENDER" with
  | None -> failwith "SMTP_SENDER not found in configuration"
  | Some sender ->
    let { email_subject; email_text; _ } = template in
    let mail =
      Sihl_email.
        { sender
        ; recipient = Pool_user.EmailAddress.value email
        ; subject = email_subject
        ; text = "" (* TODO: Plaintext *)
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

(** TODO

    - reschedule sessions

    Maybe group email params by entity not email template?? *)
module AssignmentConfirmation = struct
  let email_params lang session =
    "sessionOverview", Session.to_email_text lang session
  ;;

  let email_params_public_session lang session =
    "sessionOverview", Session.public_to_email_text lang session
  ;;

  let template pool language =
    Repo.find_by_label pool language Label.AssignmentConfirmation
  ;;

  let base_params contact = contact.Contact.user |> global_params

  let create pool language tenant session contact =
    let open Utils.Lwt_result.Infix in
    let* template = template pool language in
    let params = email_params language session :: base_params contact in
    let layout = layout_from_tenant tenant in
    let email = contact |> Contact.email_address in
    prepare_email language template email layout params |> Lwt_result.return
  ;;

  let create_from_public_session pool language tenant session contact =
    let open Utils.Lwt_result.Infix in
    let* template = template pool language in
    let params =
      email_params_public_session language session :: base_params contact
    in
    let layout = layout_from_tenant tenant in
    let email = contact |> Contact.email_address in
    prepare_email language template email layout params |> Lwt_result.return
  ;;
end

module EmailVerification = struct
  let email_params validation_url contact =
    [ "verificationUrl", validation_url ] @ global_params contact.Contact.user
  ;;

  let create pool language layout contact email_address token =
    let open Message_utils in
    let open Utils.Lwt_result.Infix in
    let* template = Repo.find_by_label pool language Label.EmailVerification in
    let%lwt url = Pool_tenant.Url.of_pool pool in
    let validation_url =
      Pool_common.[ Message.Field.Token, Email.Token.value token ]
      |> Pool_common.Message.add_field_query_params "/email-verified"
      |> create_public_url url
    in
    prepare_email
      language
      template
      email_address
      (create_layout layout)
      (email_params validation_url contact)
    |> Lwt_result.return
  ;;
end

module ExperimentInvitation = struct
  let email_params experiment public_url contact =
    let open Experiment in
    [ ( "experimentUrl"
      , create_public_url
          public_url
          (Format.asprintf
             "experiment/%s"
             (experiment.Experiment.id |> Id.value)) )
    ]
    @ experiment_params experiment
    @ global_params contact.Contact.user
  ;;

  let prepare_template_list tenant =
    let open Message_utils in
    let open Utils.Lwt_result.Infix in
    let pool = tenant.Pool_tenant.database_label in
    let%lwt system_languages = Settings.find_languages pool in
    let* templates =
      Lwt_list.map_s
        (fun lang ->
          Repo.find_by_label pool lang Label.ExperimentInvitation
          >|+ CCPair.make lang)
        system_languages
      ||> CCResult.flatten_l
    in
    let%lwt tenant_url = Pool_tenant.Url.of_pool pool in
    let layout = layout_from_tenant tenant in
    let fnc experiment (contact : Contact.t) =
      let open CCResult in
      let open Pool_common in
      let* language = message_langauge system_languages contact in
      let* template =
        CCList.find_opt (fun t -> t |> fst |> Language.equal language) templates
        |> CCOption.to_result (Message.NotFound Field.Template)
        >|= snd
      in
      let params = email_params experiment tenant_url contact in
      prepare_email
        language
        template
        (Contact.email_address contact)
        layout
        params
      |> CCResult.pure
    in
    Lwt_result.return fnc
  ;;

  let create tenant experiment contact =
    let open Message_utils in
    let open Utils.Lwt_result.Infix in
    let pool = tenant.Pool_tenant.database_label in
    let%lwt system_languages = Settings.find_languages pool in
    let* language =
      message_langauge system_languages contact |> Lwt_result.lift
    in
    let* template =
      Repo.find_by_label pool language Label.ExperimentInvitation
    in
    let%lwt tenant_url = Pool_tenant.Url.of_pool pool in
    let layout = layout_from_tenant tenant in
    let params = email_params experiment tenant_url contact in
    prepare_email
      language
      template
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
    prepare_email language template email layout (email_params user)
    |> Lwt_result.return
  ;;
end

module PasswordReset = struct
  let email_params reset_url user =
    [ "resetUrl", reset_url; "name", user |> Pool_user.user_fullname ]
  ;;

  let create pool language layout user =
    let open Message_utils in
    let open Utils.Lwt_result.Infix in
    let email = Pool_user.user_email_address user in
    let* template = Repo.find_by_label pool language Label.PasswordReset in
    let%lwt url = Pool_tenant.Url.of_pool pool in
    let* reset_token =
      Service.PasswordReset.create_reset_token
        ~ctx:(Pool_tenant.to_ctx pool)
        (Pool_user.EmailAddress.value email)
      ||> function
      | None ->
        Logs.err (fun m -> m "Reset token not found");
        Error Pool_common.Message.PasswordResetFailMessage
      | Some token -> Ok token
    in
    let reset_url =
      Pool_common.
        [ Message.Field.Token, reset_token
        ; ( Message.Field.Language
          , language |> Pool_common.Language.show |> CCString.lowercase_ascii )
        ]
      |> Pool_common.Message.add_field_query_params "/reset-password/"
      |> prepend_root_directory pool
      |> create_public_url url
    in
    prepare_email
      language
      template
      email
      (create_layout layout)
      (email_params reset_url user)
    |> Lwt_result.return
  ;;
end

module SessionCancellation = struct
  let email_params lang session reason contact =
    [ "sessionOverview", Session.to_email_text lang session
    ; "reason", reason |> Session.CancellationReason.value
    ]
    @ global_params contact.Contact.user
  ;;

  let prepare_template_list pool tenant system_languages session =
    let open Message_utils in
    let open Utils.Lwt_result.Infix in
    let* templates =
      Lwt_list.map_s
        (fun lang ->
          Repo.find_by_label pool lang Label.SessionCancellation
          >|+ CCPair.make lang)
        system_languages
      ||> CCResult.flatten_l
    in
    let layout = layout_from_tenant tenant in
    let fnc reason (contact : Contact.t) =
      let open CCResult in
      let open Pool_common in
      let* language = message_langauge system_languages contact in
      let* template =
        CCList.find_opt (fun t -> t |> fst |> Language.equal language) templates
        |> CCOption.to_result (Message.NotFound Field.Template)
        >|= snd
      in
      let params = email_params language session reason contact in
      prepare_email
        language
        template
        (Contact.email_address contact)
        layout
        params
      |> CCResult.pure
    in
    Lwt_result.return fnc
  ;;
end

module SessionReminder = struct
  let email_params lang experiment session contact =
    (("sessionOverview", Session.to_email_text lang session)
    :: experiment_params experiment)
    @ global_params contact.Contact.user
  ;;

  let create pool tenant system_languages experiment session contact =
    let open Message_utils in
    let open Utils.Lwt_result.Infix in
    let* language =
      message_langauge system_languages contact |> Lwt_result.lift
    in
    let* template = Repo.find_by_label pool language Label.SessionReminder in
    let layout = layout_from_tenant tenant in
    let params = email_params language experiment session contact in
    prepare_email
      language
      template
      (Contact.email_address contact)
      layout
      params
    |> Lwt_result.return
  ;;
end

module SessionReschedule = struct
  let email_params lang session new_start new_duration contact =
    let open Pool_common.Utils.Time in
    let open Session in
    [ "sessionOverview", to_email_text lang session
    ; "newStart", new_start |> Start.value |> formatted_date_time
    ; "newDuration", new_duration |> Duration.value |> formatted_timespan
    ]
    @ global_params contact.Contact.user
  ;;

  let prepare_template_list pool tenant system_languages session =
    let open Message_utils in
    let open Utils.Lwt_result.Infix in
    let* templates =
      Lwt_list.map_s
        (fun lang ->
          Repo.find_by_label pool lang Label.SessionReschedule
          >|+ CCPair.make lang)
        system_languages
      ||> CCResult.flatten_l
    in
    let layout = layout_from_tenant tenant in
    let fnc (contact : Contact.t) new_start new_duration =
      let open CCResult in
      let open Pool_common in
      let* language = message_langauge system_languages contact in
      let* template =
        CCList.find_opt (fun t -> t |> fst |> Language.equal language) templates
        |> CCOption.to_result (Message.NotFound Field.Template)
        >|= snd
      in
      let params =
        email_params language session new_start new_duration contact
      in
      prepare_email
        language
        template
        (Contact.email_address contact)
        layout
        params
      |> CCResult.pure
    in
    Lwt_result.return fnc
  ;;
end

module SignUpVerification = struct
  let email_params validation_url firstname lastname =
    let open Pool_user in
    [ "verificationUrl", validation_url
    ; ( "name"
      , Format.asprintf
          "%s %s"
          (Firstname.value firstname)
          (Lastname.value lastname) )
    ]
  ;;

  let create pool language tenant email_address token firstname lastname =
    let open Message_utils in
    let open Utils.Lwt_result.Infix in
    let* template = Repo.find_by_label pool language Label.SignUpVerification in
    let%lwt url = Pool_tenant.Url.of_pool pool in
    let validation_url =
      Pool_common.[ Message.Field.Token, Email.Token.value token ]
      |> Pool_common.Message.add_field_query_params "/email-verified"
      |> create_public_url url
    in
    prepare_email
      language
      template
      email_address
      (layout_from_tenant tenant)
      (email_params validation_url firstname lastname)
    |> Lwt_result.return
  ;;
end
