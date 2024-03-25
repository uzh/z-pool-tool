(* TODO: Actual mail content *)

let seed_match_filter_update_notification_template =
  Sihl.Database.Migration.create_step
    ~label:"seed match_filter_update_notification template"
    {sql|
    INSERT INTO `pool_message_templates` (`uuid`, `label`, `language`, `email_subject`, `email_text_html`, `email_text_plain`, `sms_text`) VALUES
      (UNHEX(REPLACE(UUID(), '-', '')), 'match_filter_update_notification', 'DE', 'Betreff', '<h4>Liebe*r {name},</h4><br /><p>Freundliche Grüsse<br>{siteTitle}</p>', 'Liebe*r {name},\r\n\r\n\r\n\r\nFreundliche Grüsse\r\n{siteTitle}', 'Liebe*r {name},\r\n\r\n\r\n\r\nFreundliche Grüsse\r\n{siteTitle}'),
      (UNHEX(REPLACE(UUID(), '-', '')), 'match_filter_update_notification', 'EN', 'Subject', '<h4>Dear {name},</h4><br /><p>Yours sincerely,<br>{siteTitle}</p>', 'Dear {name},\r\n\r\n\r\n\r\nYours sincerely,\r\n{siteTitle}', 'Dear {name},\r\n\r\n\r\n\r\nYours sincerely,\r\n{siteTitle}');
  |sql}
;;

let migration () =
  Sihl.Database.Migration.(
    empty "202403251311"
    |> add_step seed_match_filter_update_notification_template)
;;
