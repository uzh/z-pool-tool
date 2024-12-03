open Pool_message.Success

let src = Logs.Src.create "handler.root.settings"
let active_navigation = "/root/settings/smtp"
let show_smtp = Admin_settings_smtp.smtp_form `Root
let create_smtp = Admin_settings.Smtp.create_post `Root
let update_smtp = Admin_settings.Smtp.update_base `Root `UpdateDetails SmtpDetailsUpdated

let update_smtp_password =
  Admin_settings.Smtp.update_base `Root `UpdatePassword SmtpPasswordUpdated
;;

let delete_smtp = Admin_settings_smtp.delete_base `Root
let validate = Admin_settings_smtp.validate `Root

module Access = Admin_settings.Smtp.Access
