let add_active_after_import_column =
  Database.Migration.Step.create
    ~label:"add active_after_import to pool_user_imports"
    {sql|
      ALTER TABLE pool_user_imports
        ADD COLUMN active_after_import TINYINT(1) NOT NULL DEFAULT 1
    |sql}
;;

let seed_user_import_inactive_templates =
  Database.Migration.Step.create
    ~label:"seed user_import_inactive message templates"
    {sql|
    INSERT INTO `pool_message_templates` (`uuid`, `label`, `language`, `email_subject`, `email_text_html`, `email_text_plain`, `sms_text`) VALUES
      (UNHEX(REPLACE(UUID(), '-', '')), 'user_import_inactive', 'EN', 'Set up your account', '<div><h4>Dear {name},</h4>\n <p>Your account was recently migrated.<br/>Follow this \n  <a href="{confirmationUrl}">link</a> to set up your password.\n </p>\n <p>\n  If the above link does not work, please copy the following link into your\n  browser manually: {confirmationUrl}\n </p><p>Yours sincerely,<br/>{siteTitle}</p>\n</div>', 'Dear {name},\n\nYour account was recently migrated. Follow this link to set up your password:\n\n{confirmationUrl}\n\nYours sincerely,\n{siteTitle}', 'Dear {name},\n\nYour account was recently migrated. Follow this link to set up your password:\n\n{confirmationUrl}\n\nYours sincerely,\n{siteTitle}'),
      (UNHEX(REPLACE(UUID(), '-', '')), 'user_import_inactive', 'DE', 'Passwort einrichten', '<div><h4>Liebe*r {name},</h4>\n <p>Ihr Account wurde kürzlich migriert.<br/>Nutzen Sie diesen\n  <a href="{confirmationUrl}"> Link </a>\n  um Ihr Passwort einzurichten.\n </p>\n <p>\n  Falls der obige Link nicht funktioniert, kopieren Sie bitte den folgenden\n  manuell in Ihren Browser: {confirmationUrl}\n </p><p>Freundliche Grüsse<br/>{siteTitle}</p>\n</div>', 'Liebe*r {name},\n\nIhr Account wurde kürzlich migriert. Nutzen Sie den folgenden Link um Ihr Passwort einzurichten:\n\n{confirmationUrl}\n\nFreundliche Grüsse\n{siteTitle}', 'Liebe*r {name},\n\nIhr Account wurde kürzlich migriert. Nutzen Sie den folgenden Link um Ihr Passwort einzurichten:\n\n{confirmationUrl}\n\nFreundliche Grüsse\n{siteTitle}')
    |sql}
;;

let migration () =
  Database.Migration.(
    empty "202604130000"
    |> add_step add_active_after_import_column
    |> add_step seed_user_import_inactive_templates)
;;
