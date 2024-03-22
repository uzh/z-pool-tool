(* TODO: Fix message content and subject *)
let seed_matcher_notification_template =
  Sihl.Database.Migration.create_step
    ~label:"seed matcher notification template"
    {sql|
    INSERT INTO `pool_message_templates` (`uuid`, `label`, `language`, `email_subject`, `email_text_html`, `email_text_plain`, `sms_text`) VALUES
      (UNHEX(REPLACE(UUID(), '-', '')), 'matcher_notification', 'DE', 'Betreff', '<h4>Liebe*r {name},</h4><br /><p>Freundliche Grüsse<br>{siteTitle}</p>', 'Liebe*r {name},\r\n\r\n\r\n\r\nFreundliche Grüsse\r\n{siteTitle}', 'Liebe*r {name},\r\n\r\n\r\n\r\nFreundliche Grüsse\r\n{siteTitle}'),
      (UNHEX(REPLACE(UUID(), '-', '')), 'matcher_notification', 'EN', 'Subject', '<h4>Dear {name},</h4><br /><p>Yours sincerely,<br>{siteTitle}</p>', 'Dear {name},\r\n\r\n\r\n\r\nYours sincerely,\r\n{siteTitle}', 'Dear {name},\r\n\r\n\r\n\r\nYours sincerely,\r\n{siteTitle}');
  |sql}
;;

let add_notification_sent_flag_to_experiment =
  Sihl.Database.Migration.create_step
    ~label:"add matcher_notification_sent flag to experiment"
    {sql|
      ALTER TABLE pool_experiments
        ADD COLUMN matcher_notification_sent boolean DEFAULT false AFTER hide_in_stats
  |sql}
;;

let migration () =
  Sihl.Database.Migration.(
    empty "202403221034"
    |> add_step seed_matcher_notification_template
    |> add_step add_notification_sent_flag_to_experiment)
;;
