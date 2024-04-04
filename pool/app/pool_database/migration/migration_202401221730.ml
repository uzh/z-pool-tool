let convert_assistant_role_hint_to_i18n_en =
  Sihl.Database.Migration.create_step
    ~label:"convert assistant role hint to i18n: EN"
    {sql|
      INSERT INTO `pool_i18n` (`uuid`, `i18n_key`, `language`, `content`) VALUES (
        UNHEX(REPLACE(UUID(), '-', '')),
        'assistant_role_hint',
        'EN',
        '<p>As an assistant of the recruiter, they have several additional rights to help with administrative tasks, e.g. handling the telephone screening for waiting list experiments or running and closing the sessions.<br>An assistant can read the entire user profile for the attendance on the experiment he/she`s assigned to.</p><p>A detailed list with all role permissions is available for recruiters only.</p>'
      )
    |sql}
;;

let convert_assistant_role_hint_to_i18n_de =
  Sihl.Database.Migration.create_step
    ~label:"convert assistant role hint to i18n: DE"
    {sql|
      INSERT INTO `pool_i18n` (`uuid`, `i18n_key`, `language`, `content`) VALUES (
        UNHEX(REPLACE(UUID(), '-', '')),
        'assistant_role_hint',
        'DE',
        '<p>Als Assistent des <i>Recruiters</i> haben sie verschiedene zusätzliche Rechte, um bei administrativen Aufgaben zu helfen, z.B. beim Telefon-Screening von Experimenten auf der Warteliste oder bei der Durchführung und Schliessung von Sessions.<br>Ein Assistent kann das vollständige Benutzerprofil für die Teilnahme an dem Experiment lesen, dem er/sie zugewiesen ist.</p><p>Eine detaillierte Liste mit allen Rollenberechtigungen ist nur für Recruiter verfügbar.</p>'
      )
    |sql}
;;

let convert_experimenter_role_hint_to_i18n_en =
  Sihl.Database.Migration.create_step
    ~label:"convert experimenter role hint to i18n: EN"
    {sql|
      INSERT INTO `pool_i18n` (`uuid`, `i18n_key`, `language`, `content`) VALUES (
        UNHEX(REPLACE(UUID(), '-', '')),
        'experimenter_role_hint',
        'EN',
        '<p>An experimenter has limited rights. The role can mainly just read the information and is allowed to close sessions.<br>The experimenter can read the names of attending contacts to the experiment he/she`s assigned to.</p><p>A detailed list with all role permissions is available for recruiters only.</p>'
      )
    |sql}
;;

let convert_experimenter_role_hint_to_i18n_de =
  Sihl.Database.Migration.create_step
    ~label:"convert experimenter role hint to i18n: DE"
    {sql|
      INSERT INTO `pool_i18n` (`uuid`, `i18n_key`, `language`, `content`) VALUES (
        UNHEX(REPLACE(UUID(), '-', '')),
        'experimenter_role_hint',
        'DE',
        '<p>Ein Experimentator hat nur eingeschränkte Rechte, die Rolle kann hauptsächlich nur die Informationen lesen und darf Sitzungen schließen.<br>Der Experimentator kann die Namen der teilnehmenden Kontakte des Experiments lesen, dem er/sie zugewiesen ist.</p><p>Eine detaillierte Liste mit allen Rollenberechtigungen ist nur für Recruiter verfügbar.</p>'
      )
    |sql}
;;

let migration () =
  Sihl.Database.Migration.(
    empty "202401221730"
    |> add_step convert_assistant_role_hint_to_i18n_de
    |> add_step convert_assistant_role_hint_to_i18n_en
    |> add_step convert_experimenter_role_hint_to_i18n_de
    |> add_step convert_experimenter_role_hint_to_i18n_en)
;;
