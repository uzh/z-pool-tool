let change_disable_user_after_from_days_to_weeks =
  Sihl.Database.Migration.create_step
    ~label:"change disable user after from days to weeks"
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
    |> add_step change_disable_user_after_from_days_to_weeks)
;;
