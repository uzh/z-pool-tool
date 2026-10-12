let create_about_page_i18n =
  Database.Migration.Step.create
    ~label:"create about page i18n"
    {sql|
    INSERT INTO `pool_i18n` (`uuid`, `i18n_key`, `language`, `content`) VALUES
      (UNHEX(REPLACE(UUID(), '-', '')), 'about_page_title', 'EN', NULL),
      (UNHEX(REPLACE(UUID(), '-', '')), 'about_page_title', 'DE', NULL),
      (UNHEX(REPLACE(UUID(), '-', '')), 'about_page_text', 'EN', NULL),
      (UNHEX(REPLACE(UUID(), '-', '')), 'about_page_text', 'DE', NULL)
      ON DUPLICATE KEY UPDATE id = id;
  |sql}
;;

let migration () =
  Database.Migration.(empty "202610121032" |> add_step create_about_page_i18n)
;;
