open Entity_i18n

let to_string = function
  | DashboardTitle -> "Dashboard"
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
  | FollowUpSessionFor -> "Folgesession für:"
  | ExperimentListTitle -> "Experimente"
  | ExperimentWaitingListTitle -> "Warteliste"
  | HomeTitle -> "Willkommen beim Pool Tool"
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
  | LoginTitle -> "Anmelden"
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
  | SessionSignUpTitle -> "Für diese Session anmelden"
  | SignUpAcceptTermsAndConditions -> "Ich akzeptiere die Nutzungsbedingungen."
  | SignUpTitle -> "Registrieren"
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
  | UserProfileTitle -> "Benutzerprofil"
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
  | Tenants -> "Tenants"
  | WaitingList -> "Warteliste"
;;

let hint_to_string = function
  | AllowUninvitedSignup ->
    "Kontakte, die nicht eingeladen wurden, können sich für das Experiment \
     anmelden."
  | AssignContactFromWaitingList ->
    "Wählen Sie die Session, zu welcher Sie den Kontakt zuweisen wollen."
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
  | CustomHtmx s -> s
  | DirectRegistrationDisbled ->
    "Ist diese Option aktiviert, können sich Kontakte auf die Warteliste \
     setzen, aber nicht direkt für das Experiment einschreiben."
  | Distribution ->
    "Mit der Verteilung kann beeinflusst werden, welche Einladungen als erstes \
     versendet werden."
  | I18nText str -> str
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
  | SessionReminderLanguageHint ->
    "Falls sie einen eigenen Erinnerungstext angeben, wählen Sie dessen \
     Sprache hier."
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
   | CancelSession -> "die Session", "absagen"
   | DeleteCustomFieldOption -> "das Option", "löschen"
   | DeleteEmailSuffix -> "das Suffix", "löschen"
   | DeleteExperiment -> "das Experiment", "löschen"
   | DeleteFile -> "die Datei", "löschen"
   | DeleteMailing -> "den Versand", "löschen"
   | DeleteSession -> "die Session", "löschen"
   | StopMailing -> "den Versand", "stoppen")
  |> fun (obj, action) ->
  Format.asprintf "Sind Sie sicher, dass Sie %s %s wollen?" obj action
;;
