let insert_disabled_inactive_user_service_settings =
  Database.Migration.Step.create
    ~label:"seed default system settings"
    {sql|
      INSERT INTO pool_system_settings (uuid, settings_key, value) VALUES
      (UNHEX(REPLACE(UUID(), '-', '')), '[\"inactive_user_service_disabled\"]', 'true')
      ON DUPLICATE KEY UPDATE id = id;
    |sql}
;;

let migration () =
  Database.Migration.(
    empty "202502100919" |> add_step insert_disabled_inactive_user_service_settings)
;;
