let seed_matcher_notification_template =
  Sihl.Database.Migration.create_step
    ~label:"seed matcher notification template"
    {sql|
    INSERT INTO `pool_message_templates` (`uuid`, `label`, `LANGUAGE`, `email_subject`, `email_text_html`, `email_text_plain`, `sms_text`) VALUES
    (UNHEX(REPLACE(UUID(), '-', '')), 'matcher_notification', 'DE', '{experimentPublicTitle}: Alle passenden Kontakte eingeladen', '<h4>Liebe*r {name}</h4><p>Alle Kontakte, die die Kriterien des Experiments <a href=\"{experimentUrl}\">{experimentPublicTitle}</a>, wurden eingeladen, es gibt jedoch noch freue Plätze.<br><br>Freundliche Grüsse<br>{siteTitle}</p>', 'Liebe*r {name}\r\n\r\nAlle Kontakte, die die Kriterien des Experiments {experimentPublicTitle}, wurden eingeladen, es gibt jedoch noch freue Plätze.\r\n\r\nFreundliche Grüsse\r\n{siteTitle}', 'Liebe*r {name}\r\n\r\nAlle Kontakte, die die Kriterien des Experiments {experimentPublicTitle}, wurden eingeladen, es gibt jedoch noch freue Plätze.\r\n\r\nFreundliche Grüsse\r\n{siteTitle}'),
    (UNHEX(REPLACE(UUID(), '-', '')), 'matcher_notification', 'EN', '{experimentPublicTitle}: All matching contacts invited', '<p>Dear {name}</p><p>All contacts who meet the criteria of the experiment <a href=\"{experimentId}\">{experimentPublicTitle}</a> have been invited, but there are still places available.</p><p>Kind regards<br>{siteTitle}</p>', 'Dear {name}\r\n\r\nAll contacts who meet the criteria of the experiment {experimentPublicTitle} have been invited, but there are still places available.\r\n\r\nKind regards\r\n{siteTitle}', 'Dear {name}\r\n\r\nAll contacts who meet the criteria of the experiment {experimentPublicTitle} have been invited, but there are still places available.\r\n\r\nKind regards\r\n{siteTitle}');
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
