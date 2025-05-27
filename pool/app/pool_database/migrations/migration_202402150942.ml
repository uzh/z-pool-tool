let add_actor_permission_create_hint_en =
  Database.Migration.Step.create
    ~label:"add actor permission hint to i18n: EN"
    {sql|
      INSERT INTO `pool_i18n` (`uuid`, `i18n_key`, `language`, `content`) VALUES (
        UNHEX(REPLACE(UUID(), '-', '')),
        'actor_permission_create_hint',
        'EN',
        '<p>Search for admins and "click" add to provide them "Create `Experiment" permission.</p>'
      )
    |sql}
;;

let add_actor_permission_create_hint_de =
  Database.Migration.Step.create
    ~label:"add actor permission hint to i18n: DE"
    {sql|
      INSERT INTO `pool_i18n` (`uuid`, `i18n_key`, `language`, `content`) VALUES (
        UNHEX(REPLACE(UUID(), '-', '')),
        'actor_permission_create_hint',
        'DE',
        '<p>Suche Administratoren und füge diese hinzu. Diese verfügen anschliessend über die Rechte um Experimente zu erstellen.</p>'
      )
    |sql}
;;

let add_actor_permission_hint_en =
  Database.Migration.Step.create
    ~label:"add actor permission hint to i18n: EN"
    {sql|
      INSERT INTO `pool_i18n` (`uuid`, `i18n_key`, `language`, `content`) VALUES (
        UNHEX(REPLACE(UUID(), '-', '')),
        'actor_permission_hint',
        'EN',
        '<p>Instead of assigning a role with a predefined set of authorizations to a user, user-specific permissions can be created. This list shows all user-specific actor permissions (can have duplicates). When deleting one of the duplicates, all other duplicates are deleted as well.</p>'
      )
    |sql}
;;

let add_actor_permission_hint_de =
  Database.Migration.Step.create
    ~label:"add actor permission hint to i18n: DE"
    {sql|
      INSERT INTO `pool_i18n` (`uuid`, `i18n_key`, `language`, `content`) VALUES (
        UNHEX(REPLACE(UUID(), '-', '')),
        'actor_permission_hint',
        'DE',
        '<p>Anstatt einem Benutzer eine Rolle mit einem vordefinierten Satz von Berechtigungen zuzuweisen, können benutzerspezifische Berechtigungen erstellt werden. Diese Liste zeigt alle benutzerspezifischen Berechtigungen (kann duplikate enthalten). Wenn eines der Duplikate gelöscht wird, werden auch alle anderen Duplikate gelöscht.</p>'
      )
    |sql}
;;

let migration () =
  Database.Migration.(
    empty "202402150942"
    |> add_step add_actor_permission_create_hint_en
    |> add_step add_actor_permission_create_hint_de
    |> add_step add_actor_permission_hint_en
    |> add_step add_actor_permission_hint_de)
;;
