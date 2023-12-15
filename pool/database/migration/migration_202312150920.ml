let seed_default_signup_cta_i18n =
  Sihl.Database.Migration.create_step
    ~label:"seed root default i18n data"
    {sql|
    INSERT INTO pool_i18n (uuid, i18n_key, language, content) VALUES
      (UNHEX(REPLACE(UUID(), '-', '')), 'signupcta', 'EN', 'Register now to participate in economic experiments.'),
      (UNHEX(REPLACE(UUID(), '-', '')), 'signupcta', 'DE', 'Jetzt anmelden und an Experimenten teilnehmen.')
      ON DUPLICATE KEY UPDATE id = id;
  |sql}
;;

let migration () =
  Sihl.Database.Migration.(
    empty "202312150920" |> add_step seed_default_signup_cta_i18n)
;;
