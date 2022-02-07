open Entity
module User = Pool_user
module Database = Pool_database

let create_public_url pool_url path =
  path
  |> Sihl.Web.externalize_path
  |> Format.asprintf "%s%s" (Pool_tenant.Url.value pool_url)
;;

let prepend_root_directory pool url =
  match Database.Label.equal pool Database.root with
  | true -> Format.asprintf "/root%s" url
  | false -> url
;;

let prepare_email pool template_label subject email params =
  let%lwt template =
    Service.EmailTemplate.get_by_label
      ~ctx:(Pool_tenant.to_ctx pool)
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
  let create pool language ~user =
    let email = user.Sihl_user.email in
    let%lwt url = Pool_tenant.Url.of_pool pool in
    let%lwt reset_token =
      Service.PasswordReset.create_reset_token
        ~ctx:(Pool_tenant.to_ctx pool)
        email
    in
    match reset_token with
    | None -> Lwt.return_error Pool_common.Message.PasswordResetFailMessage
    | Some token ->
      let subject = "Password reset" in
      let reset_url =
        Pool_common.
          [ Message.Token, token
          ; ( Message.Language
            , language |> Pool_common.Language.code |> CCString.lowercase_ascii
            )
          ]
        |> Pool_common.Message.add_field_query_params "/reset-password/"
        |> prepend_root_directory pool
        |> Sihl.Web.externalize_path
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
        (User.Firstname.value firstname)
        (User.Lastname.value lastname)
    in
    let subject = "Password has been changed" in
    prepare_email
      db_pool
      "password_change"
      subject
      (address email |> Pool_user.EmailAddress.value)
      [ "name", name ]
  ;;
end

module ConfirmationEmail = struct
  let create pool email firstname lastname event =
    let%lwt url = Pool_tenant.Url.of_pool pool in
    let name =
      CCString.concat
        " "
        (CCList.filter_map
           CCFun.id
           [ firstname |> CCOption.map Pool_user.Firstname.value
           ; lastname |> CCOption.map Pool_user.Lastname.value
           ])
    in
    let subject = "Email verification" in
    let validation_url =
      Pool_common.[ Message.Token, token email ]
      |> Pool_common.Message.add_field_query_params "/reset-password/"
      |> Sihl.Web.externalize_path
      |> create_public_url url
    in
    let create_email template =
      prepare_email
        pool
        template
        subject
        (address email |> Pool_user.EmailAddress.value)
        [ "verificationUrl", validation_url; "name", name ]
    in
    create_email
    @@
    match event with
    | `SignUp -> "signup_verification"
    | `EmailUpdate -> "email_verification"
  ;;
end
