let seed_manual_message_templates =
  Sihl.Database.Migration.create_step
    ~label:"seed default message templates"
    {sql|
    INSERT INTO `pool_message_templates` (`uuid`, `label`, `language`, `email_subject`, `email_text_html`, `email_text_plain`, `sms_text`) VALUES
      (UNHEX(REPLACE(UUID(), '-', '')), 'manual_session_message', 'DE', 'Betreff', '<h4>Liebe*r {name},</h4><br /><p>Freundliche Grüsse<br>{siteTitle}</p>', 'Liebe*r {name},\r\n\r\n\r\n\r\nFreundliche Grüsse\r\n{siteTitle}', 'Liebe*r {name},\r\n\r\n\r\n\r\nFreundliche Grüsse\r\n{siteTitle}'),
      (UNHEX(REPLACE(UUID(), '-', '')), 'manual_session_message', 'EN', 'Subject', '<h4>Dear {name},</h4><br /><p>Yours sincerely,<br>{siteTitle}</p>', 'Dear {name},\r\n\r\n\r\n\r\nYours sincerely,\r\n{siteTitle}', 'Dear {name},\r\n\r\n\r\n\r\nYours sincerely,\r\n{siteTitle}');
  |sql}
;;

let migration () =
  Sihl.Database.Migration.(
    empty "202403131128" |> add_step seed_manual_message_templates)
;;
