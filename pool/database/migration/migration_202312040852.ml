let store_settings_timespans_in_seconds =
  Sihl.Database.Migration.create_step
    ~label:"store settings timespans in seconds"
    {sql|
      UPDATE
        pool_system_settings
      SET
        value = value * 86400
      WHERE
        settings_key IN ('["inactive_user_disable_after"]', '["inactive_user_warning"]', '["trigger_profile_update_after"]' )
    |sql}
;;

let migration () =
  Sihl.Database.Migration.(
    empty "202312040852" |> add_step store_settings_timespans_in_seconds)
;;
