let insert_system_email_templates_setting =
  Database.Migration.Step.create
    ~label:"seed system_email_templates setting"
    {sql|
      INSERT INTO pool_system_settings (uuid, settings_key, value) VALUES
      (
        UNHEX(REPLACE(UUID(), '-', '')),
        '["system_email_templates"]',
        '["login_2fa_token","password_change","password_reset"]'
      )
      ON DUPLICATE KEY UPDATE id = id;
    |sql}
;;

let migration () =
  Database.Migration.(
    empty "202603131200" |> add_step insert_system_email_templates_setting)
;;
