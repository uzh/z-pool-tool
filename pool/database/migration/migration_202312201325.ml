let convert_settings_terms_to_i18n =
  Sihl.Database.Migration.create_step
    ~label:"convert settings terms to i18n"
    {sql|
    INSERT INTO pool_i18n (uuid, i18n_key, language, content) VALUES
      (UNHEX(REPLACE(UUID(), '-', '')), 'terms_and_conditions', 'EN', 'Nutzungsbedingungen'),
      (UNHEX(REPLACE(UUID(), '-', '')), 'terms_and_conditions', 'DE', 'Terms and conditions')
      ON DUPLICATE KEY UPDATE id = id;
  |sql}
;;

let delete_settings_entries =
  Sihl.Database.Migration.create_step
    ~label:"convert settings terms to i18n"
    {sql|
      DELETE FROM pool_system_settings WHERE settings_key = "[\"terms_and_conditions\"]"
  |sql}
;;

let migration () =
  Sihl.Database.Migration.(
    empty "202312201325"
    |> add_step convert_settings_terms_to_i18n
    |> add_step delete_settings_entries)
;;
