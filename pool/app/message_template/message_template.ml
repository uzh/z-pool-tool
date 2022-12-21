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
      (("emailText", email_text) :: params) @ Message_utils.layout_params layout
    in
    Sihl_email.Template.email_of_template mail params
;;

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
    prepare_email language template email layout params |> Lwt_result.ok
  ;;

  let create_from_public_session pool language tenant session contact =
    let open Utils.Lwt_result.Infix in
    let* template = template pool language in
    let params =
      email_params_public_session language session :: base_params contact
    in
    let layout = Message_utils.layout_from_tenant tenant in
    let email = contact |> Contact.email_address in
    prepare_email language template email layout params |> Lwt_result.ok
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
    |> Lwt_result.ok
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
    |> Lwt_result.ok
  ;;
end
