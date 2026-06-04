let insert_profile_only =
  Database.Migration.Step.create
    ~label:"add ProfileOnly system setting"
    {sql|
      INSERT INTO pool_system_settings (uuid, settings_key, value) VALUES
        (UNHEX(REPLACE(UUID(), '-', '')), '["profile_only"]', 'false')
      ON DUPLICATE KEY UPDATE id = id;
    |sql}
;;

let insert_dashboard_intro =
  Database.Migration.Step.create
    ~label:"add DashboardIntro i18n key"
    {sql|
      INSERT INTO pool_i18n (uuid, i18n_key, language, content) VALUES
        (UNHEX(REPLACE(UUID(), '-', '')), 'dashboard_intro', 'EN', ''),
        (UNHEX(REPLACE(UUID(), '-', '')), 'dashboard_intro', 'DE', '')
      ON DUPLICATE KEY UPDATE id = id;
    |sql}
;;

let migration () =
  Database.Migration.(
    empty "202605291200"
    |> add_step insert_profile_only
    |> add_step insert_dashboard_intro)
;;
