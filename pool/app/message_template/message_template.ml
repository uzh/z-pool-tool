include Entity
include Event
include Default

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
        ; html = Some (Message_utils.combine_html language (Some email_subject))
        ; cc = []
        ; bcc = []
        }
    in
    let params =
      [ "emailText", email_text ] @ Message_utils.layout_params layout @ params
    in
    Sihl_email.Template.render_email_with_data params mail
;;

(** TODO

    - ExperimentInvitation
    - SessionCancellation
    - SessionReminder *)
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

  let base_params contact = [ "name", contact |> Contact.fullname ]

  let create pool language tenant session contact =
    let open Utils.Lwt_result.Infix in
    let* template = template pool language in
    let params = email_params language session :: base_params contact in
    let layout = Message_utils.layout_from_tenant tenant in
    let email = contact |> Contact.email_address in
    prepare_email language template email layout params |> Lwt_result.return
  ;;

  let create_from_public_session pool language tenant session contact =
    let open Utils.Lwt_result.Infix in
    let* template = template pool language in
    let params =
      email_params_public_session language session :: base_params contact
    in
    let layout = Message_utils.layout_from_tenant tenant in
    let email = contact |> Contact.email_address in
    prepare_email language template email layout params |> Lwt_result.return
  ;;
end

module EmailVerification = struct
  let email_params validation_url contact =
    [ "verificationUrl", validation_url; "name", Contact.fullname contact ]
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
    [ "experimentPublicTitle", public_title_value experiment
    ; "experimentDescription", description_value experiment
    ; ( "experimentUrl"
      , Message_utils.create_public_url
          public_url
          (Format.asprintf "experiment/%s" (experiment.id |> Id.value)) )
    ; "name", Contact.fullname contact
    ]
  ;;

  let get_langauge sys (contact : Contact.t) default =
    let open CCOption in
    let open Pool_common in
    contact.Contact.language
    >>= (fun contact_lang -> CCList.find_opt (Language.equal contact_lang) sys)
    |> value ~default
  ;;

  let prepare_template_list tenant =
    let open Message_utils in
    let open Utils.Lwt_result.Infix in
    let pool = tenant.Pool_tenant.database_label in
    let%lwt system_languages = Settings.find_languages pool in
    let* default_language =
      system_languages
      |> CCList.head_opt
      |> CCOption.to_result Pool_common.Message.(Retrieve Field.Language)
      |> Lwt_result.lift
    in
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
      let language = get_langauge system_languages contact default_language in
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
    let* default_language =
      system_languages
      |> CCList.head_opt
      |> CCOption.to_result Pool_common.Message.(Retrieve Field.Language)
      |> Lwt_result.lift
    in
    let language = get_langauge system_languages contact default_language in
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
  let email_params user = [ "name", user |> Pool_user.user_fullname ]

  let create pool language tenant user =
    let open Utils.Lwt_result.Infix in
    let* template = Repo.find_by_label pool language Label.PasswordChange in
    let layout = Message_utils.layout_from_tenant tenant in
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
