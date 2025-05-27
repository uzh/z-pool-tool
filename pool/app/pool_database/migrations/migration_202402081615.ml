let insert_user_import_interval_default_values =
  Database.Migration.Step.create
    ~label:"insert user import interval default values"
    {sql|
      INSERT INTO pool_system_settings (`uuid`, `settings_key`, `value`) VALUES (
        UNHEX(REPLACE(UUID(), '-', '')),
        '["user_import_first_reminder_after"]',
        '604800'), (
        UNHEX(REPLACE(UUID(), '-', '')),
        '["user_import_second_reminder_after"]',
        '604800'
        )
    |sql}
;;

let migration () =
  Database.Migration.(
    empty "202402081615" |> add_step insert_user_import_interval_default_values)
;;
