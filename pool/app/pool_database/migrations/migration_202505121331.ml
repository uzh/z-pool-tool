let create_authentication_table =
  Database.Migration.Step.create
    ~label:"create pool_authentication table"
    {sql|
      CREATE TABLE IF NOT EXISTS pool_authentication (
        `id` bigint(20) unsigned NOT NULL AUTO_INCREMENT,
        `uuid` binary(16) NOT NULL,
        `user_uuid` binary(16) NOT NULL,
        `token` varchar(128) NOT NULL,
        `channel` enum('email') NOT NULL,
        `valid_until` timestamp NOT NULL,
        `created_at` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP,
        `updated_at` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
      PRIMARY KEY (id),
      UNIQUE KEY `unique_uuid` (`uuid`),
      UNIQUE KEY `unique_user_uuid` (`user_uuid`),
      FOREIGN KEY (user_uuid) REFERENCES user_users(uuid)
      ) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci
    |sql}
;;

let seed_login_token_template =
  Database.Migration.Step.create
    ~label:"seed login_token_template"
    {sql|
    INSERT INTO `pool_message_templates` (`uuid`, `label`, `language`, `email_subject`, `email_text_html`, `email_text_plain`, `sms_text`) VALUES
      (UNHEX(REPLACE(UUID(), '-', '')),'login_2fa_token', 'DE', 'Ihr Verifizierungstoken', '<h4>Grüezi {name}</h4><p>Hier ist Ihr Login-Verifizierungscode:</p><p><strong>{token}</strong></p><p>Der Code ist für 5 Minuten gültig.</p><p>Wenn Sie nicht kürzlich versucht haben, sich anzumelden, ist Ihr Konto möglicherweise gefährdet. Bitte setzen Sie Ihr Passwort sofort zurück.</p><p>Freundliche Grüsse<br>{siteTitle}</p>', 'Grüezi {name}\r\n\r\nHier ist Ihr Verifizierungscode: {token}\r\n\r\nDer Code ist für 5 Minuten gültig.\r\n\r\nWenn Sie nicht kürzlich versucht haben, sich anzumelden, ist Ihr Konto möglicherweise gefährdet. Bitte setzen Sie Ihr Passwort sofort zurück.\r\n\r\nFreundliche Grüsse\r\n{siteTitle}', 'Grüezi {name}\r\nHier ist Ihr Verifizierungscode: {token}\r\nWenn Sie nicht kürzlich versucht haben, sich anzumelden, setzen Sie Ihr Passwort zurück.\r\nFreundliche Grüsse'),
      (UNHEX(REPLACE(UUID(), '-', '')),'login_2fa_token', 'EN', 'Your verification token', '<h4>Dear {name}</h4><p>Here is your login verification code:</p><p><strong>{token}</strong></p><p>The code is valid for 5 minutes.</p><p>If you did not recently attempt to login, your account may be compromised. Please reset your password immediately.</p><p>Best Regards<br>{siteTitle}</p>', 'Dear {name}\r\n\r\nHere is your login verification code: {token}\r\n\r\nThe code is valid for 5 minutes.\r\n\r\nIf you did not recently attempt to login, your account may be compromised. Please reset your password immediately.\r\n\r\nBest Regards\r\n{siteTitle}', 'Hello {name}\r\nHere is your verification code: {token}\r\nIf you have not tried to log in recently, please reset your password.\r\nKind regards');
  |sql}
;;

(* This migrations are executed on root and tenant databases *)
let migration () =
  Database.Migration.(
    empty "202505121331"
    |> add_step create_authentication_table
    |> add_step seed_login_token_template)
;;
