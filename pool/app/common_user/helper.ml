open Entity

let create_public_url pool_url path =
  path
  |> Sihl.Web.externalize_path
  |> Format.asprintf "%s%s" (Pool_common.Url.value pool_url)
;;

module Email = struct
  let prepare_email pool template_label subject email params =
    let%lwt template =
      Service.EmailTemplate.get_by_label
        ~ctx:(Pool_common.Utils.pool_to_ctx pool)
        template_label
    in
    match template, Sihl.Configuration.read_string "SMTP_SENDER" with
    | _, None -> failwith "SMTP_SENDER not found in configuration"
    | None, _ -> failwith "Email template not found!"
    | Some template, Some sender ->
      let mail =
        Sihl_email.
          { sender
          ; recipient = email
          ; subject
          ; text = ""
          ; html = None
          ; cc = []
          ; bcc = []
          }
      in
      Sihl_email.Template.email_of_template ~template mail params
  ;;

  module PasswordReset = struct
    let create pool ~user =
      let email = user.Sihl_user.email in
      let%lwt url = Pool_common.Repo.Url.of_pool pool in
      let%lwt reset_token =
        Service.PasswordReset.create_reset_token
          ~ctx:(Pool_common.Utils.pool_to_ctx pool)
          email
      in
      match reset_token with
      | None -> Lwt.return_error Pool_common.Message.PasswordResetFailMessage
      | Some token ->
        let subject = "Password reset" in
        let reset_url =
          Format.asprintf "/reset-password/?token=%s" token
          |> create_public_url url
        in
        let given_name =
          user.Sihl_user.given_name |> CCOption.value ~default:""
        in
        let name = user.Sihl_user.name |> CCOption.value ~default:"" in
        prepare_email
          pool
          "password_reset"
          subject
          email
          [ "resetUrl", reset_url
          ; "name", Format.asprintf "%s %s" given_name name
          ]
        |> Lwt_result.ok
    ;;
  end

  module PasswordChange = struct
    let create db_pool email firstname lastname =
      let name =
        Format.asprintf
          "%s %s"
          (Firstname.value firstname)
          (Lastname.value lastname)
      in
      let subject = "Password has been changed" in
      prepare_email
        db_pool
        "password_change"
        subject
        (Email.address email)
        [ "name", name ]
    ;;
  end

  module ConfirmationEmail = struct
    let create db_pool email firstname lastname =
      let%lwt url = Pool_common.Repo.Url.of_pool db_pool in
      let name =
        Format.asprintf
          "%s %s"
          (Firstname.value firstname)
          (Lastname.value lastname)
      in
      let subject = "Email verification" in
      let validation_url =
        Format.asprintf "/email-verified?token=%s" (Email.token email)
        |> create_public_url url
      in
      prepare_email
        db_pool
        "email_verification"
        subject
        (Email.address email)
        [ "verificationUrl", validation_url; "name", name ]
    ;;
  end
end
