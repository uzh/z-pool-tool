let seed_assignment_cancellation_templates =
  Database.Migration.create_step
    ~label:"seed default message templates"
    {sql|
    INSERT INTO `pool_message_templates` (`uuid`, `label`, `language`, `email_subject`, `email_text_html`, `email_text_plain`, `sms_text`) VALUES
      (UNHEX(REPLACE(UUID(), '-', '')), 'assignment_cancellation', 'DE', 'Anmeldungsannulierung', '<h4>Liebe*r {name},</h4><p>Deine Anmeldung zur Teilnahme am Experiment {experimentPublicTitle} am {sessionStart} wurde annulliert.</p><p>Freundliche Grüsse<br>{siteTitle}</p>', 'Liebe*r {name},\r\n\r\nDeine Anmeldung zur Teilnahme am Experiment {experimentPublicTitle} am {sessionStart} wurde annulliert.\r\n\r\nFreundliche Grüsse\r\n{siteTitle}', 'Liebe*r {name},\r\n\r\nDeine Anmeldung zur Teilnahme am Experiment {experimentPublicTitle} am {sessionStart} wurde annulliert.\r\n\r\nFreundliche Grüsse\r\n{siteTitle}'),
      (UNHEX(REPLACE(UUID(), '-', '')), 'assignment_cancellation', 'EN', 'Assignment cancellation', '<h4>Dear {name},</h4><p>Your registration to participate in the experiment {experimentPublicTitle} on {sessionStart} has been canceled.</p><p>Yours sincerely,<br>{siteTitle}</p>', 'Dear {name},\r\n\r\nYour registration to participate in the experiment {experimentPublicTitle} on {sessionStart} has been canceled.\r\n\r\nYours sincerely,\r\n{siteTitle}', 'Dear {name},\r\n\r\nYour registration to participate in the experiment {experimentPublicTitle} on {sessionStart} has been canceled.\r\n\r\nYours sincerely,\r\n{siteTitle}');
  |sql}
;;

let migration () =
  Sihl.Database.Migration.(
    empty "202402161523" |> add_step seed_assignment_cancellation_templates)
;;
