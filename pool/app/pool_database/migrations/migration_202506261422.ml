let update_login_token_template_en =
  Database.Migration.Step.create
    ~label:"update login_token_template"
    {sql|
    UPDATE `pool_message_templates`
    SET `email_subject` = 'Your {siteTitle} Verification Code',
        `email_text_html` = '<p>🔐 Your Verification Code</p><p>Please use the following code to complete your sign-in:</p><div class="otp apple-otp">{token}</div><p>This code will expire in 5 minutes.</p><p>Sent by <strong>{siteUrl}. </strong>If you did not recently attempt to login, your account may be compromised. Please reset your password immediately.</p><p>Best Regards<br>{siteTitle}</p>',
        `email_text_plain` = '🔐 Your Verification Code

Please use the following code to complete your sign-in:

{token}

This code will expire in 5 minutes.

Sent by {siteUrl}. If you did not recently attempt to login, your account may be compromised. Please reset your password immediately.

Best Regards
{siteTitle}',
        `sms_text` = 'Your verification code is: {token}
This code will expire in 5 minutes. Do not share it with anyone.
If you have not tried to log in recently, please reset your password.
Sent by {siteUrl}'
    WHERE `label` = 'login_2fa_token' AND `language` = 'EN' and `entity_uuid` IS NULL;
  |sql}
;;

let update_login_token_template_de =
  Database.Migration.Step.create
    ~label:"update login_token_template"
    {sql|
    UPDATE `pool_message_templates`
    SET `email_subject` = 'Ihr {siteTitle} Verifizierungscode',
        `email_text_html` = '<p>🔐 Ihr Verifizierungscode</p><p>Bitte verwenden Sie den folgenden Code, um Ihre Anmeldung abzuschließen:</p><div class="otp apple-otp">{token}</div><p>Dieser Code ist 5 Minuten lang gültig.</p><p>Gesendet von <strong>{siteUrl}</strong>. Wenn Sie sich nicht kürzlich angemeldet haben, könnte Ihr Konto kompromittiert worden sein. Bitte setzen Sie Ihr Passwort umgehend zurück.</p><p>Beste Grüsse<br />{siteTitle}</p>',
        `email_text_plain` = '🔐 Ihr Verifizierungscode

Bitte verwenden Sie den folgenden Code, um Ihre Anmeldung abzuschließen:

{token}

Dieser Code ist 5 Minuten lang gültig.

Gesendet von <strong>{siteUrl}</strong>. Wenn Sie sich nicht kürzlich angemeldet haben, könnte Ihr Konto kompromittiert worden sein. Bitte setzen Sie Ihr Passwort umgehend zurück.

Beste Grüsse
{siteTitle}',
        `sms_text` = 'Ihr Verifizierungscode ist: {token}
Der ist 5 Minuten lang gültig. Teilen Sie ihn mit niemandem.
Wenn Sie sich nicht kürzlich angemeldet haben, setzen Sie bitte Ihr Passwort umgehend zurück.
Gesendet von {siteUrl}'
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
