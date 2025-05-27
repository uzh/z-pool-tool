let seed_inactive_contact_message_templates =
  Database.Migration.Step.create
    ~label:"seed account deactivation notification templates"
    {sql| INSERT INTO `pool_message_templates` (uuid, label, language, email_subject, email_text_html, email_text_plain, sms_text) VALUES (
            UNHEX(REPLACE(UUID(), '-', '')),
            'inactive_contact_warning',
             'DE',
             'Ihr Konto wird aufgrund Inaktivität bald deaktiviert',
            '<h4>Grüezi {name},</h4><p>Sie haben sich seit dem {lastLogin} nicht mehr in Ihren Account eingeloggt.&nbsp;</p><p>Falls Sie sich nicht wieder einloggen, wird Ihr Account pausiert und Sie werden an keine weiteren Experimente mehr eingeladen.&nbsp;</p><p>Freundliche Grüsse&nbsp;<br>{siteTitle}</p>',
            'Grüezi {name},\r\n\r\nSie haben sich seit dem {lastLogin} nicht mehr in Ihren Account eingeloggt. \r\n\r\nFalls Sie sich nicht wieder einloggen, wird Ihr Account pausiert und Sie werden an keine weiteren Experimente mehr eingeladen. \r\n\r\nFreundliche Grüsse \r\n{siteTitle}',
            'Grüezi {name},\r\n\r\nIhr Account wird demnächst deaktiviert, falls Sie sich nicht mehr einloggen.\r\n\r\nFreundliche Grüsse\r\n{siteTitle}'
          ), (
            UNHEX(REPLACE(UUID(), '-', '')),
            'inactive_contact_warning',
            'EN',
            'Your account will soon be deactivated due to inactivity',
            '<h4>Dear {name},</h4><p>You have not logged into your account since {lastLogin}.</p><p>If you do not log in again, your account will be paused and you will not be invited to any further experiments.&nbsp;</p><p>Yours sincerely,<br>{siteTitle}</p>',
            'Dear {name},\r\n\r\nYou have not logged into your account since {lastLogin}.\r\n\r\nIf you do not log in again, your account will be paused and you will not be invited to any further experiments. \r\n\r\nYours sincerely,\r\n{siteTitle}',
            'Dear {name},\r\n\r\nYour account will be deactivated soon, if you do not log in again.\r\n\r\nYours sincerely,\r\n\r\n{siteTitle}'
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

let make_reminder_setting_a_list =
  Database.Migration.Step.create
    ~label:"make reminder after setting a list"
    {sql|
      UPDATE pool_system_settings
      SET value = CONCAT('[', value, ']')
      WHERE settings_key = "[\"inactive_user_warning\"]";
    |sql}
;;

let create_pool_contact_deactivation_notification_table =
  Database.Migration.Step.create
    ~label:"create pool_contact_deactivation_notification table"
    {sql|
     CREATE TABLE IF NOT EXISTS pool_contact_deactivation_notification (
        id BIGINT UNSIGNED AUTO_INCREMENT,
        contact_uuid BINARY(16) NOT NULL,
        created_at timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP,
        updated_at timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
        PRIMARY KEY (id)
      ) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci
    |sql}
;;

let add_pool_contact_deactivation_notification_fk =
  Database.Migration.Step.create
    ~label:"add pool_contact_deactivation_notification fk"
    {sql|
      ALTER TABLE pool_contact_deactivation_notification
      ADD CONSTRAINT fk_contact_uuid
      FOREIGN KEY (contact_uuid)
      REFERENCES user_users(uuid)
      ON DELETE CASCADE;
    |sql}
;;

let migration () =
  Database.Migration.(
    empty "202412170838"
    |> add_step seed_inactive_contact_message_templates
    |> add_step make_reminder_setting_a_list
    |> add_step create_pool_contact_deactivation_notification_table
    |> add_step add_pool_contact_deactivation_notification_fk)
;;
