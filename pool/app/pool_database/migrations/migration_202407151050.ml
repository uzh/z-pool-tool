let create_customizable_terminology_i18n =
  Database.Migration.Step.create
    ~label:"create customizable terminology i18n"
    {sql|
    INSERT INTO `pool_i18n` (`uuid`, `i18n_key`, `language`, `content`) VALUES
      (UNHEX(REPLACE(UUID(), '-', '')), 'dashboard_upcoming_sessions', 'EN', 'Your upcoming sessions'),
      (UNHEX(REPLACE(UUID(), '-', '')), 'dashboard_upcoming_sessions', 'DE', 'Ihre nächsten Sessions'),
      (UNHEX(REPLACE(UUID(), '-', '')), 'dashboard_online_studies', 'EN', 'Available online surveys'),
      (UNHEX(REPLACE(UUID(), '-', '')), 'dashboard_online_studies', 'DE', 'Verfügbare Onlinestudien'),
      (UNHEX(REPLACE(UUID(), '-', '')), 'dashboard_experiment_registration', 'EN', 'Registering for experiment sessions'),
      (UNHEX(REPLACE(UUID(), '-', '')), 'dashboard_experiment_registration', 'DE', 'Neuanmeldung zu Experiment-Sessions'),
      (UNHEX(REPLACE(UUID(), '-', '')), 'dashboard_experiment_history', 'EN', 'Experiment history'),
      (UNHEX(REPLACE(UUID(), '-', '')), 'dashboard_experiment_history', 'DE', 'Experimenthistorie'),
      (UNHEX(REPLACE(UUID(), '-', '')), 'dashboard_waiting_list', 'EN', 'On the waiting list'),
      (UNHEX(REPLACE(UUID(), '-', '')), 'dashboard_waiting_list', 'DE', 'Auf der Warteliste'),
      (UNHEX(REPLACE(UUID(), '-', '')), 'experiment_navigation_title', 'EN', 'Experiments'),
      (UNHEX(REPLACE(UUID(), '-', '')), 'experiment_navigation_title', 'DE', 'Experimente')
      ON DUPLICATE KEY UPDATE id = id;
  |sql}
;;

let migration () =
  Database.Migration.(
    empty "202407151050" |> add_step create_customizable_terminology_i18n)
;;
