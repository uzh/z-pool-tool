let insert_phone_verification_enabled_setting =
  Database.Migration.Step.create
    ~label:"seed phone verification enabled setting"
    {sql|
      INSERT INTO pool_system_settings (uuid, settings_key, value)
      VALUES (UNHEX(REPLACE(UUID(), '-', '')), '["phone_verification_enabled"]', 'true')
      ON DUPLICATE KEY UPDATE id = id
    |sql}
;;

let migration () =
  Database.Migration.(
    empty "202604170000" |> add_step insert_phone_verification_enabled_setting)
;;
