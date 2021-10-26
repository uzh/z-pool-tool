let prepare_email pool template_label subject email params =
  let%lwt template =
    Service.EmailTemplate.get_by_label
      ~ctx:[ "pool", pool |> Pool_common.Database.Label.value ]
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
  let create pool msg ~user =
    let email = user.Sihl_user.email in
    let%lwt reset_token =
      Service.PasswordReset.create_reset_token
        ~ctx:[ "pool", pool |> Pool_common.Database.Label.value ]
        ~email
    in
    match reset_token with
    | None -> Lwt.return_error msg
    | Some token ->
      let subject = "Password reset" in
      let reset_url =
        Format.asprintf "/reset-password/?token=%s" token
        |> Sihl.Web.externalize_path
        |> Utils.Url.create_public_url
      in
      let username = user.Sihl_user.username |> Option.value ~default:"User" in
      prepare_email
        pool
        "password_reset"
        subject
        email
        [ "resetUrl", reset_url; "username", username ]
      |> Lwt_result.ok
  ;;
end
