let insert_system_email_templates_setting =
  Database.Migration.Step.create
    ~label:"seed system_email_templates setting"
    {sql|
      INSERT INTO pool_system_settings (uuid, settings_key, value) VALUES
      (
        UNHEX(REPLACE(UUID(), '-', '')),
        '["system_email_templates"]',
        '[["account_suspension_notification"],["contact_email_change_attempt"],["contact_registration_attempt"],["email_verification"],["inactive_contact_warning"],["inactive_contact_deactivation"],["login_2fa_token"],["matcher_notification"],["match_filter_update_notification"],["password_change"],["password_reset"],["phone_verification"],["profile_update_trigger"],["signup_verification"],["user_import"]]'
      )
      ON DUPLICATE KEY UPDATE id = id;
    |sql}
;;

let migration () =
  Database.Migration.(
    empty "202603131200" |> add_step insert_system_email_templates_setting)
;;
