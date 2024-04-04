let add_home_title_to_welcome_text_i18n_en =
  Sihl.Database.Migration.create_step
    ~label:"add home title to welcome text i18n: EN"
    {sql|
      UPDATE pool_i18n
      SET content = CONCAT('<h2><strong>University Registration Center for Study Participants</h2></strong>', content)
      WHERE i18n_key = 'welcome_text'
        AND language = 'EN'
    |sql}
;;

let add_home_title_to_welcome_text_i18n_de =
  Sihl.Database.Migration.create_step
    ~label:"add home title to welcome text i18n: DE"
    {sql|
      UPDATE pool_i18n
      SET content = CONCAT('<h2><strong>Universitäre Anmeldestelle für Studienteilnehmende</h2></strong>', content)
      WHERE i18n_key = 'welcome_text'
        AND language = 'DE'
    |sql}
;;

let migration () =
  Sihl.Database.Migration.(
    empty "202401081554"
    |> add_step add_home_title_to_welcome_text_i18n_en
    |> add_step add_home_title_to_welcome_text_i18n_de)
;;
