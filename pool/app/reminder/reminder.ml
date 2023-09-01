module Service = Service

let prepare_messages database_label tenant tenant_languages experiment session =
  let%lwt email_reminders =
    Message_template.SessionReminder.prepare_emails
      database_label
      tenant
      tenant_languages
      experiment
      session
  in
  let%lwt text_message_reminders =
    Message_template.SessionReminder.prepare_text_messages
      database_label
      tenant
      tenant_languages
      experiment
      session
  in
  Lwt.return (email_reminders, text_message_reminders)
;;
