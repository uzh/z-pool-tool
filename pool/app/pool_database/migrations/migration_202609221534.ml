let create_customizable_unsubscribe_i18n =
  Database.Migration.Step.create
    ~label:"create customizable unsubscribe i18n"
    {sql|
    INSERT INTO `pool_i18n` (`uuid`, `i18n_key`, `language`, `content`) VALUES
      (UNHEX(REPLACE(UUID(), '-', '')), 'unsubscribe_title', 'EN', 'Unsubscribe from experiment invitations'),
      (UNHEX(REPLACE(UUID(), '-', '')), 'unsubscribe_title', 'DE', 'Abmelden'),
      (UNHEX(REPLACE(UUID(), '-', '')), 'unsubscribe_text', 'EN', '<p>The email address "{email}" is unsubscribed from receiving experiment invitations. You can reactivate it in your account settings.</p><p>Are you sure you want to unsubscribe from the experiment invitations?</p>'),
      (UNHEX(REPLACE(UUID(), '-', '')), 'unsubscribe_text', 'DE', '<p>Die E-Mail-Adresse "{email}" wird künftig keine Experimenteinladungen mehr erhalten. Sie können dies in Ihren Kontoeinstellungen wieder aktivieren.</p><p>Sind Sie sicher, dass Sie künftig keine Einladungen mehr erhalten wollen?</p>')
      ON DUPLICATE KEY UPDATE id = id;
  |sql}
;;

let migration () =
  Database.Migration.(
    empty "202609221534" |> add_step create_customizable_unsubscribe_i18n)
;;
