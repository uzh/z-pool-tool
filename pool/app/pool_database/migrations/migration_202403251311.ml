let seed_match_filter_update_notification_template =
  Database.Migration.Step.create
    ~label:"seed match_filter_update_notification template"
    {sql|
    INSERT INTO `pool_message_templates` (`uuid`, `label`, `language`, `email_subject`, `email_text_html`, `email_text_plain`, `sms_text`) VALUES
      (UNHEX(REPLACE(UUID(), '-', '')), 'match_filter_update_notification', 'DE', 'Kontakte entsprechen nicht mehr den Experimentierkriterien', '<h4>Liebe*r {name}</h4><p>Folgende angemeldete Personen erfüllen nicht mehr die Kriterien, um am Experiment {experimentPublicTitle} teilzunehmen:</p><p>{assignments}<br><br>Freundliche Grüsse<br>{siteTitle}</p>', 'Liebe*r {name}\r\n\r\nFolgende angemeldete Personen erfüllen nicht mehr die Kriterien, um am Experiment {experimentPublicTitle} teilzunehmen:\r\n\r\n{assignments}\r\n\r\nFreundliche Grüsse\r\n{siteTitle}', 'Liebe*r {name},\r\n\r\neinige angemeldete Personen erfüllen nicht mehr die Kriterien, an dem Experiment {experimentPublicTitle} teilzunehmen.\r\n\r\nFreundliche Grüsse\r\n{siteTitle}'),
      (UNHEX(REPLACE(UUID(), '-', '')), 'match_filter_update_notification', 'EN', 'Contacts no longer meet experiment criteria', '<h4>Dear {name},</h4><p>The following registered contacts no longer meet the criteria to participate in the experiment {experimentPublicTitle}:<br><br>{assignments}</p><p>Yours sincerely,<br>{siteTitle}</p>', 'Dear {name},\r\n\r\nThe following registered contacts no longer meet the criteria to participate in the experiment {experimentPublicTitle}:\r\n\r\n{assignments}\r\n\r\nYours sincerely,\r\n\r\n{siteTitle}', 'Dear {name},\r\n\r\nThe following registered contacts no longer meet the criteria to participate in the experiment {experimentPublicTitle}:\r\n\r\n{assignments}\r\n\r\nYours sincerely,\r\n\r\n{siteTitle}');
  |sql}
;;

let migration () =
  Database.Migration.(
    empty "202403251311" |> add_step seed_match_filter_update_notification_template)
;;
