let seed_inactive_contact_message_templates =
  Database.Migration.Step.create
    ~label:"seed account deactivation notification templates"
    {sql| INSERT INTO `pool_message_templates` (uuid, label, language, email_subject, email_text_html, email_text_plain, sms_text) VALUES (
            UNHEX(REPLACE(UUID(), '-', '')),
            'inactive_contact_warning',
             'DE',
             'Ihr Konto wird aufgrund Inaktivität bald deaktiviert',
            '<h4>Grüezi {name},</h4><p>Sie haben sich seit dem {lastLogin} nicht mehr in Ihren Account eingeloggt.&nbsp;</p><p>Falls Sie sich bis zum {deactivationAt} nicht einloggen, wird Ihr Account pausiert und Sie werden an keine weiteren Experimente mehr eingeladen.&nbsp;</p><p>Freundliche Grüsse&nbsp;<br>{siteTitle}</p>',
            'Grüezi {name},\r\n\r\nSie haben sich seit dem {lastLogin} nicht mehr in Ihren Account eingeloggt. \r\n\r\nFalls Sie sich bis zum {deactivationAt} nicht einloggen, wird Ihr Account pausiert und Sie werden an keine weiteren Experimente mehr eingeladen. \r\n\r\nFreundliche Grüsse \r\n{siteTitle}',
            'Grüezi {name},\r\n\r\nIhr Account wird am {deactivationAt} deaktiviert, falls Sie sich nicht mehr einloggen.\r\n\r\nFreundliche Grüsse\r\n{siteTitle}'
          ), (
            UNHEX(REPLACE(UUID(), '-', '')),
            'inactive_contact_warning',
            'EN',
            'Your account will soon be deactivated due to inactivity',
            '<h4>Dear {name},</h4><p>You have not logged into your account since {lastLogin}.</p><p>If you do not log in by {deactivationAt}, your account will be paused and you will not be invited to any further experiments.&nbsp;</p><p>Yours sincerely,<br>{siteTitle}</p>',
            'Dear {name},\r\n\r\nYou have not logged into your account since {lastLogin}.\r\n\r\nIf you do not log in by {deactivationAt}, your account will be paused and you will not be invited to any further experiments. \r\n\r\nYours sincerely,\r\n{siteTitle}',
            'Dear {name},\r\n\r\nYour account will be deactivated on the {deactivationAt}, if you do not log in again.\r\n\r\nYours sincerely,\r\n\r\n{siteTitle}'
          ), (
            UNHEX(REPLACE(UUID(), '-', '')),
            'inactive_contact_deactivation',
            'DE',
            'Ihr Konto wurde aufgrund Inaktivität deaktiviert',
            '<h4>Grüezi {name},</h4><p>Ihr Account wurde aufgrund von Inaktivität deaktiviert. Sie werden an keine weiteren Experimente mehr eingeladen.</p><p>Sie können Ihren Account unter <a href=\"{siteUrl}\">{siteUrl}</a> reaktivieren.</p><p>Freundliche Grüsse&nbsp;<br>{siteTitle}</p>',
            'Grüezi {name},\r\n\r\nIhr Account wurde aufgrund von Inaktivität deaktiviert. Sie werden an keine weiteren Experimente mehr eingeladen.\r\n\r\nSie können Ihren Account unter {siteUrl} reaktivieren.\r\n\r\nFreundliche Grüsse \r\n{siteTitle}',
            'Grüezi {name},\r\n\r\nIhr Account wurde aufgrund von Inaktivität deaktiviert.\r\n\r\nFreundliche Grüsse\r\n{siteTitle}'
          ), (
            UNHEX(REPLACE(UUID(), '-', '')),
            'inactive_contact_deactivation',
            'EN',
            'Your account has been deactivated due to inactivity',
            '<h4>Dear {name},</h4><p>Your account has been deactivated due to inactivity. You will not be invited to any more experiments.</p><p>You can reactivate your account at <a href=\"{siteUrl}\">{siteUrl}</a>.</p><p>Yours sincerely,<br>{siteTitle}</p>',
            'Dear {name},\r\n\r\nYour account has been deactivated due to inactivity. You will not be invited to any more experiments.\r\n\r\nYou can reactivate your account at {siteUrl}.\r\n\r\nYours sincerely,\r\n{siteTitle}',
            'Dear {name},\r\n\r\nYour account has been deactivated due to inactivity. You can reactivate your account at {siteUrl}.\r\n\r\nYours sincerely,\r\n{siteTitle}'
          ); 
        |sql}
;;

let migration () =
  Database.Migration.(
    empty "202412170838" |> add_step seed_inactive_contact_message_templates)
;;
