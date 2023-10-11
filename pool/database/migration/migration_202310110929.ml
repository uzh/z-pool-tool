let seed_password_change_attempt_templates =
  Sihl.Database.Migration.create_step
    ~label:"seed waiting list confirmation"
    {sql|
    INSERT INTO `pool_message_templates` (`uuid`, `label`, `language`, `email_subject`, `email_text_html`, `email_text_plain`, `sms_text`) VALUES
      (UNHEX(REPLACE(UUID(), '-', '')), 'contact_password_change_attempt', 'EN', 'Attempt to update email address', '<div><h4>Dear {name}</h4><p>There was an attempt to change the email address on an account to '{emailAddress}' on {tenantUrl}. An account with this email address already exists.</p><p>If this was you and you forgot your login credentials, you can reset your password here: {resetUrl}</p><p>You can ignore this message or inform the administrators if you did not perform this action.</p><p>Best Regards<br/>{siteTitle}</p></div>', 'Dear {name}\n\nThere was an attempt to change the email address on an account to '{emailAddress}' on {tenantUrl}. An account with this email address already exists.\n\nIf this was you and you forgot your login credentials, you can reset your password here: {resetUrl}\n\nYou can ignore this message or inform the administrators if you did not perform this action.\n\nBest Regards\n\n{siteTitle}'),
      (UNHEX(REPLACE(UUID(), '-', '')), 'contact_password_change_attempt', 'DE', 'Versuch, die E-Mail-Adresse zu aktualisieren', '<div><h4>Grüezi {name}</h4><p>Es wurde versucht auf {tenantUrl} die Email-Adresse eines Kontos auf '{emailAddress}' zu ändern. Es existiert bereits ein Konto mit dieser E-Mail-Adresse.</p><p>Wenn Sie dies waren und Ihre Anmeldedaten vergessen haben, können Sie Ihr Passwort hier zurücksetzen: {resetUrl}</p><p>Wenn diese Aktion nicht von Ihnen durchgeführt wurde, können Sie diese Meldung ignorieren oder die Administratoren informieren.</p><p>nFreundliche Grüsse<br/>{siteTitle}</p></div>', 'Grüezi {name}\n\nEs wurde versucht auf {tenantUrl} die Email-Adresse eines Kontos auf '{emailAddress}' zu ändern. Es existiert bereits ein Konto mit dieser E-Mail-Adresse.\n\nWenn Sie dies waren und Ihre Anmeldedaten vergessen haben, können Sie Ihr Passwort hier zurücksetzen: {resetUrl}\n\nWenn diese Aktion nicht von Ihnen durchgeführt wurde, können Sie diese Meldung ignorieren oder die Administratoren informieren.\n\nFreundliche Grüsse\n{siteTitle}')
  |sql}
;;

let migration () =
  Sihl.Database.Migration.(
    empty "202310110929" |> add_step seed_password_change_attempt_templates)
;;
