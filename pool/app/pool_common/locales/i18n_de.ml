open Entity_i18n

let to_string = function
  | DashboardTitle -> "Dashboard"
  | DontHaveAnAccount -> "Noch kein Zugang?"
  | EmailConfirmationNote ->
    "Bitte prüfen Sie zunächst Ihre E-Mails und bestätigen Sie Ihre Adresse."
  | EmailConfirmationTitle -> "Bestätigung Ihrer Email Adresse"
  | ExperimentNewTitle -> "Neues Experiment erstellen"
  | ExperimentSessionReminderHint ->
    "Dies sind Standardeinstellungen für die Sessions dieses Experiment. Diese \
     Einstellungen können pro Session angepasst werden."
  | EmtpyList field ->
    Format.asprintf
      "Es sind keine %s vorhanden."
      (Locales_de.field_to_string field)
  | ExperimentContactEnrolledNote ->
    "Sie sind an der folgenden Session angemeldet:"
  | Files -> "Dateien"
  | FilterNrOfContacts ->
    "Anzahl der Kontakte, die den Kriterien dieses Filters entsprechen:"
  | FollowUpSessionFor -> "Folgesession für:"
  | ExperimentListTitle -> "Experimente"
  | ExperimentWaitingListTitle -> "Warteliste"
  | HomeTitle -> "Universitäre Anmeldestelle für Studienteilnehmende"
  | I18nTitle -> "Übersetzungen"
  | NoEntries field ->
    Format.asprintf
      "Es existiert noch keine %s."
      (Locales_de.field_to_string field)
  | OurPartners -> "Unsere Partner"
  | ProfileCompletionTitle -> "Profilvervollständigung"
  | LocationFileNew -> "Neue Datei zu Standort hinzufügen"
  | LocationListTitle -> "Standorte"
  | LocationNewTitle -> "Neuer Standort erstellen"
  | LocationNoFiles -> "Es existieren keine Dateien zu diesem Standort."
  | LocationNoSessions -> "Keine Sessions für diesen Standort gefunden."
  | LoginTitle -> "Login"
  | MailingDetailTitle start ->
    Format.asprintf "Versand vom %s" (Utils_time.formatted_date_time start)
  | MailingNewTitle -> "Neuen Versand erstellen"
  | RateTotalSent number ->
    Format.asprintf "Total generierter Einladungen: %d" number
  | ResetPasswordLink | ResetPasswordTitle -> "Passwort zurücksetzen"
  | Reminder -> "Erinnerung"
  | SentInvitations -> "Versendete Einladungen"
  | SessionDetailTitle start ->
    Format.asprintf "Session am %s" (Utils_time.formatted_date_time start)
  | SessionReminderDefaultLeadTime leadtime ->
    Format.asprintf
      "Die Standardvorlaufzeit dieses Experiments ist: %s"
      (leadtime |> Pool_common_utils.Time.formatted_timespan)
  | SessionReminderDefaultText text ->
    Format.asprintf
      "Die Standarderinnerungstext dieses Experiments ist:\n %s"
      text
  | SessionReminderDefaultSubject text ->
    Format.asprintf
      "Der Standarderinnerungsbetreff dieses Experiments ist:\n %s"
      text
  | SessionReminder -> "Sessionerinnerung"
  | SessionIndent -> "Einrückungen groupieren Folgesessions."
  | SessionRegistrationTitle -> "Für diese Session anmelden"
  | SignUpAcceptTermsAndConditions -> "Ich akzeptiere die Nutzungsbedingungen."
  | SignUpCTA -> "Jetzt anmelden und an Experimenten teilnehmen."
  | SignUpTitle -> "Anmeldung"
  | SortUngroupedFields -> "Nicht gruppierte Felder sortieren"
  | SwitchChronological -> "Zu chronologische Ansicht wechseln"
  | SwitchGrouped -> "Zu gruppierter Ansicht wechseln"
  | TermsAndConditionsTitle -> "Nutzungsbedingungen"
  | TextTemplates -> "Textelemente"
  | UserProfileDetailsSubtitle -> "Persönliche Angaben"
  | UserProfileLoginSubtitle -> "Anmeldeinformationen"
  | UserProfilePausedNote ->
    "Sie haben alle Benachrichtigungen für Ihren Benutzer pausiert! (Klicken \
     Sie auf 'Bearbeiten', um diese Einstellung)"
  | Validation -> "Validierung"
  | WaitingListIsDisabled -> "Die Warteliste ist deaktiviert."
;;

let nav_link_to_string = function
  | Admins -> "Administratoren"
  | Assignments -> "Anmeldungen"
  | Contacts -> "Konktakte"
  | CustomFields -> "Felder"
  | Dashboard -> "Dashboard"
  | Experiments -> "Experimente"
  | Filter -> "Filter"
  | I18n -> "Übersetzungen"
  | Invitations -> "Einladungen"
  | Locations -> "Standorte"
  | LoginInformation -> "Anmeldeinformationen"
  | Login -> "Login"
  | Logout -> "Logout"
  | Mailings -> "Versand"
  | Overview -> "Übersicht"
  | PersonalDetails -> "Persönliche Angaben"
  | Profile -> "Profil"
  | Sessions -> "Sessions"
  | Settings -> "Einstellungen"
  | SystemSettings -> "Systemeinstellungen"
  | Tenants -> "Tenants"
  | Users -> "Benutzer"
  | WaitingList -> "Warteliste"
;;

let hint_to_string = function
  | AllowUninvitedSignup ->
    "Kontakte, die nicht eingeladen wurden, können sich für das Experiment \
     anmelden."
  | AssignContactFromWaitingList ->
    "Wählen Sie die Session, zu welcher Sie den Kontakt zuweisen wollen."
  | ContactOnWaitingList ->
    "Sie stehen auf der Warteliste. Das Rekrutierungsteam wird Sie einer \
     Session zuweisen."
  | CustomFieldAdminInputOnly ->
    Format.asprintf
      "Diese Option schliesst \"%s\" aus."
      (Locales_de.field_to_string Entity_message.Field.Required
      |> CCString.capitalize_ascii)
  | CustomFieldAdminViewOnly ->
    Format.asprintf
      "Diese Option impliziert \"%s\"."
      (Locales_de.field_to_string Entity_message.Field.AdminInputOnly
      |> CCString.capitalize_ascii)
  | CustomFieldContactModel ->
    "Fragen, die Kontakte beantworten können, bzw. müssen. Anhand dieser \
     Informationen werden die Kontakte zu Experimenten eingeladen."
  | CustomFieldExperimentModel -> "Anpassbare Attribute für Experimente."
  | CustomFieldSessionModel -> "Anpassbare Attribute für Sessions."
  | CustomFieldGroups ->
    {|Gruppen, nach denen benutzerdefinierte Felder gruppiert werden können. Das Gruppieren von benutzerdefinierten Feldern hat keine keine Auswirkungen auf ihre Funktionalität. Sie hat lediglich grafische Auswirkungen.|}
  | CustomFieldSort field ->
    Format.asprintf
      "In dieser Reihenfolge werden die %s den Kontakten angezeigt."
      (Locales_de.field_to_string field)
  | CustomHtmx s -> s
  | DirectRegistrationDisbled ->
    "Ist diese Option aktiviert, können sich Kontakte auf die Warteliste \
     setzen, aber nicht direkt für das Experiment einschreiben."
  | Distribution ->
    "Mit der Verteilung kann beeinflusst werden, welche Einladungen als erstes \
     versendet werden."
  | ExperimentAssignment ->
    "Alle Anmeldungen von Kontakten an Sessions dieses Experiments, sortiert \
     nach Session."
  | ExperimentMailings ->
    {|Einladungsversand dieses Experiments. Die 'Rate' definiert die maximal generierten Einladungen pro Stunde.

    Gestartete Mailings können nicht mehr gelöscht werden.|}
  | ExperimentWaitingList ->
    "Kontakte, die zu diesem Experiment eingeladen wurden, und sich auf die \
     Warteliste gesetzt haben. Sie müssen manuell einer Session zugewiesen \
     werden."
  | ExperimentSessions ->
    {|Alle existierenden Session dieses Experiments.
  Sobald sich jemand angemeldet hat, kann die Session nicht mehr gelöscht werden.
  |}
  | I18nText str -> str
  | LocationFiles ->
    "Zusatzinformationen zum Standort, wie z.B. eine Wegbeschreibung. \
     Kontakte, die an einer Session an diesem Standort teilnehmen, können auf \
     diese Dateien zugreiffen."
  | LocationSessions ->
    "Zukünftige Sessions, die an diesem Standort durchgeführt werden."
  | Locations ->
    "Standorte, an denen Experimente durchgeführt werden. Jede Session muss \
     eine Location haben."
  | NumberIsSecondsHint -> "Anzahl Sekunden"
  | NumberIsDaysHint -> "Anzahl Tage"
  | NumberIsWeeksHint -> "Anzahl Wochen"
  | Overbook ->
    "Anzahl Probanden, die sich zusätzlich zur maximalen Anzahl Teilnehmer, an \
     einer Session einschreiben können."
  | Rate -> "Generierte Einladungen pro Stunde"
  | RateDependencyWith ->
    "Zur selben Zeit finden weitere Versande statt, details werden unten \
     angezeigt. Die Summe aller Raten wird automatisch gedrosselt, sobald das \
     maximum des Servers erreicht wird."
  | RateDependencyWithout ->
    "Zur Zeit finden im angegebenen Zeitfenster keine weiteren Versande statt."
  | RateNumberPerMinutes (per_n_minutes, number) ->
    Format.asprintf
      "Generiert alle %d Minuten %.2f neue Einladungen."
      per_n_minutes
      number
  | RegistrationDisabled ->
    "Ist diese Option aktiviert, können sich Probanden weder anmelden noch auf \
     die Warteliste setzen. Das Experiment ist für die Kontakte nicht \
     ersichtlich."
  | SessionClose ->
    {|S: Der Kontakt ist an der Session erschienen
    P: Der Kontakt hat am Experiment teilgenommen

    Um 'participated' anzuwählen ist 'show up' erforderlich.
    |}
  | SessionReminderLanguageHint ->
    "Falls sie einen eigenen Erinnerungstext angeben, wählen Sie dessen \
     Sprache hier."
  | SessionRegistrationHint ->
    "Die Registrierung für eine Session ist verbindlich."
  | SelectedDateIsPast -> "Das gewählte Datum liegt in der Vergangenheit."
  | SignUpForWaitingList ->
    "Das Rekrutierungsteam wird sich mit Ihnen in Verbindung setzen, um Ihnen \
     einen Termin zuzuweisen, wenn ein freier Platz vorhanden ist."
  | TemplateTextElementsHint ->
    "Die folgenden Textbausteine können in den Templates verwendet werden:"
  | TimeSpanPickerHint -> "Stunden und Minuten"
;;

let confirmable_to_string confirmable =
  (match confirmable with
   | CancelSession -> "die Session", "absagen", None
   | DeleteCustomField -> "das Feld", "löschen", None
   | DeleteCustomFieldOption -> "das Option", "löschen", None
   | DeleteEmailSuffix -> "das Suffix", "löschen", None
   | DeleteExperiment -> "das Experiment", "löschen", None
   | DeleteFile -> "die Datei", "löschen", None
   | DeleteMailing -> "den Versand", "löschen", None
   | DeleteSession -> "die Session", "löschen", None
   | PublisCustomField ->
     ( "das Feld und alle dazugehörigen Optionen"
     , "publizieren"
     , Some "Sie werden das Feld nicht mehr löschen können." )
   | PublisCustomFieldOption ->
     ( "die Option"
     , "publizieren"
     , Some "Sie werden die Option nicht mehr löschen können." )
   | StopMailing -> "den Versand", "stoppen", None)
  |> fun (obj, action, additive) ->
  Format.asprintf "Sind Sie sicher, dass Sie %s %s wollen?" obj action
  |> fun msg ->
  additive
  |> CCOption.map_or ~default:msg (fun additive ->
       Format.asprintf "%s %s" msg additive)
;;
