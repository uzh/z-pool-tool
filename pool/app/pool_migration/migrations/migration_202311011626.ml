let change_disable_user_after_from_weeks_to_days =
  Database.Migration.create_step
    ~label:"change disable user after from week to days"
    {sql|
      UPDATE
        pool_system_settings
      SET
        value = value * 7
      WHERE
        settings_key = '["inactive_user_disable_after"]'
    |sql}
;;

let migration () =
  Sihl.Database.Migration.(
    empty "202311011626"
    |> add_step change_disable_user_after_from_weeks_to_days)
;;
