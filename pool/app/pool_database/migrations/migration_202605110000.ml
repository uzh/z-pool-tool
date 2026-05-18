let seed_admin_account_created_templates =
  Database.Migration.Step.create
    ~label:"seed admin_account_created message templates"
    {sql| INSERT INTO `pool_message_templates` (`uuid`, `label`, `language`, `email_subject`, `email_text_html`, `email_text_plain`, `sms_text`) VALUES (
            UNHEX(REPLACE(UUID(), '-', '')),
            'admin_account_created',
            'EN',
            'Your admin account has been created',
            '<div><h4>Dear {name},</h4><p>An admin account has been created for you.<br>To activate your account and set your password, please verify your email by following this <a href="{verificationUrl}"> link</a>.</p><p>After verification you will receive a second email with a link to set your password.</p><p>If this action wasn''t performed by you, please ignore this email or reply to let us know.</p><p>If the above link does not work, please copy the following link into your browser manually: {verificationUrl}</p><p>Yours sincerely,<br>{siteTitle}</p></div>',
            'Dear {name},\n\nAn admin account has been created for you.\nTo activate your account and set your password, please verify your email using the link below.\n\n{verificationUrl}\n\nAfter verification you will receive a second email with a link to set your password.\n\nIf this action wasn''t performed by you, please ignore this email or reply to let us know.\n\nYours sincerely,\n{siteTitle}',
            'Dear {name},\n\nAn admin account has been created for you.\nTo activate your account and set your password, please verify your email using the link below.\n\n{verificationUrl}\n\nAfter verification you will receive a second email with a link to set your password.\n\nIf this action wasn''t performed by you, please ignore this email or reply to let us know.\n\nYours sincerely,\n{siteTitle}'
          ), (
            UNHEX(REPLACE(UUID(), '-', '')),
            'admin_account_created',
            'DE',
            'Ihr Admin-Konto wurde erstellt',
            '<div><h4>Liebe*r {name},</h4><p>Für Sie wurde ein Admin-Konto erstellt.<br>Um Ihr Konto zu aktivieren und Ihr Passwort zu setzen, verifizieren Sie bitte Ihre E-Mail-Adresse, indem Sie diesem <a href="{verificationUrl}"> Link</a> folgen.</p><p>Nach der Verifizierung erhalten Sie eine zweite E-Mail mit einem Link zum Setzen Ihres Passworts.</p><p>Falls diese Aktion nicht von Ihnen durchgeführt wurde, ignorieren Sie bitte diese E-Mail oder antworten Sie, um uns zu informieren.</p><p>Falls der obige Link nicht funktioniert, kopieren Sie bitte den folgenden Link manuell in Ihren Browser: {verificationUrl}</p><p>Freundliche Grüsse,<br>{siteTitle}</p></div>',
            'Liebe*r {name},\n\nFür Sie wurde ein Admin-Konto erstellt.\nUm Ihr Konto zu aktivieren und Ihr Passwort zu setzen, verifizieren Sie bitte Ihre E-Mail-Adresse über den folgenden Link.\n\n{verificationUrl}\n\nNach der Verifizierung erhalten Sie eine zweite E-Mail mit einem Link zum Setzen Ihres Passworts.\n\nFalls diese Aktion nicht von Ihnen durchgeführt wurde, ignorieren Sie bitte diese E-Mail oder antworten Sie, um uns zu informieren.\n\nFreundliche Grüsse\n{siteTitle}',
            'Liebe*r {name},\n\nFür Sie wurde ein Admin-Konto erstellt.\nUm Ihr Konto zu aktivieren und Ihr Passwort zu setzen, verifizieren Sie bitte Ihre E-Mail-Adresse über den folgenden Link.\n\n{verificationUrl}\n\nNach der Verifizierung erhalten Sie eine zweite E-Mail mit einem Link zum Setzen Ihres Passworts.\n\nFalls diese Aktion nicht von Ihnen durchgeführt wurde, ignorieren Sie bitte diese E-Mail oder antworten Sie, um uns zu informieren.\n\nFreundliche Grüsse\n{siteTitle}'
          )
        |sql}
;;

let migration () =
  Database.Migration.(
    empty "202605110000" |> add_step seed_admin_account_created_templates)
;;
