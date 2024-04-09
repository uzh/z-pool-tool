let convert_settings_terms_to_i18n_en =
  Database.Migration.create_step
    ~label:"convert settings terms to i18n: EN"
    {sql|
    INSERT INTO pool_i18n (uuid, i18n_key, language, content) VALUES (
      UNHEX(REPLACE(UUID(), '-', '')),
      'terms_and_conditions',
      'EN',
      (SELECT
        SUBSTRING_INDEX(SUBSTRING_INDEX(`value`, '["EN"],"', - 1), '"]', 1)
        FROM pool_system_settings
        WHERE settings_key = "[\"terms_and_conditions\"]"));
  |sql}
;;

let convert_settings_terms_to_i18n_de =
  Database.Migration.create_step
    ~label:"convert settings terms to i18n: DE"
    {sql|
    INSERT INTO pool_i18n (uuid, i18n_key, language, content) VALUES (
      UNHEX(REPLACE(UUID(), '-', '')),
      'terms_and_conditions',
      'DE',
      (SELECT
        SUBSTRING_INDEX(SUBSTRING_INDEX(`value`, '["DE"],"', -1), '"]', 1)
        FROM pool_system_settings
        WHERE settings_key = "[\"terms_and_conditions\"]"));
  |sql}
;;

let delete_settings_entries =
  Database.Migration.create_step
    ~label:"convert settings terms to i18n"
    {sql|
      DELETE FROM pool_system_settings WHERE settings_key = "[\"terms_and_conditions\"]"
  |sql}
;;

let migration () =
  Sihl.Database.Migration.(
    empty "202312201325"
    |> add_step convert_settings_terms_to_i18n_en
    |> add_step convert_settings_terms_to_i18n_de
    |> add_step delete_settings_entries)
;;
