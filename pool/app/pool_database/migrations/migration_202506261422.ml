let update_login_token_template_en =
  Database.Migration.Step.create
    ~label:"update login_token_template"
    {sql|
    UPDATE `pool_message_templates`
    SET `email_subject` = 'Your {siteTitle} Verification Code',
        `email_text_html` = '<p>🔐 Your Verification Code</p><p>Please use the following code to complete your sign-in:</p><div class=\"otp apple-otp\">{token}</div><p>This code will expire in 5 minutes.</p><p>If you did not recently attempt to login to <strong>{siteUrl}</strong>, your account may be compromised. Please reset your password immediately and contact us.</p><p>Best Regards<br>{siteTitle}</p>',
        `email_text_plain` = '🔐 Your Verification Code\r\n\r\nPlease use the following code to complete your sign-in:\r\n\r\n{token}\r\n\r\nThis code will expire in 5 minutes.\r\n\r\nIf you did not recently attempt to login to {siteUrl}, your account may be compromised. Please reset your password immediately and contact us.\r\n\r\nBest Regards\r\n{siteTitle}',
        `sms_text` = '🔐 Your verification code is: {token}\r\nThis code will expire in 5 minutes. Do not share it with anyone.\r\nIf you have not tried to log in recently, please reset your password and contact us.\r\nSent by {siteUrl}'
    WHERE `label` = 'login_2fa_token' AND `language` = 'EN' and `entity_uuid` IS NULL;
  |sql}
;;

let update_login_token_template_de =
  Database.Migration.Step.create
    ~label:"update login_token_template"
    {sql|
    UPDATE `pool_message_templates`
    SET `email_subject` = 'Ihr {siteTitle} Verifizierungscode',
        `email_text_html` = '<p>🔐 Ihr Verifizierungscode</p><p>Bitte verwenden Sie den folgenden Code, um Ihre Anmeldung abzuschliessen:</p><div class=\"otp apple-otp\">{token}</div><p>Dieser Code ist 5 Minuten lang gültig.</p><p>Wenn Sie sich nicht kürzlich bei <strong>{siteUrl}</strong> angemeldet haben, könnte Ihr Konto kompromittiert worden sein. Bitte setzen Sie Ihr Passwort umgehend zurück und kontaktieren uns.</p><p>Beste Grüsse<br>{siteTitle}</p>',
        `email_text_plain` = '🔐 Ihr Verifizierungscode\r\n\r\nBitte verwenden Sie den folgenden Code, um Ihre Anmeldung abzuschliessen:\r\n\r\n{token}\r\n\r\nDieser Code ist 5 Minuten lang gültig.\r\n\r\nWenn Sie sich nicht kürzlich bei {siteUrl} angemeldet haben, könnte Ihr Konto kompromittiert worden sein. Bitte setzen Sie Ihr Passwort umgehend zurück und kontaktieren uns.\r\n\r\nBeste Grüsse\r\n{siteTitle}',
        `sms_text` = '🔐 Ihr Verifizierungscode ist: {token}\r\nDer ist 5 Minuten lang gültig. Teilen Sie ihn mit niemandem.\r\nWenn Sie sich nicht kürzlich angemeldet haben, setzen Sie bitte Ihr Passwort umgehend zurück und kontaktieren uns.\r\nGesendet von {siteUrl}'
    WHERE `label` = 'login_2fa_token' AND `language` = 'DE' and `entity_uuid` IS NULL;
  |sql}
;;

(* This migrations are executed on root and tenant databases *)
let migration () =
  Database.Migration.(
    empty "202506261422"
    |> add_step update_login_token_template_en
    |> add_step update_login_token_template_de)
;;
