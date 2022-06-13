open Entity_i18n

let to_string = function
  | DashboardTitle -> "Dashboard"
  | EmailConfirmationNote ->
    "Bitte prüfen Sie zunächst Ihre E-Mails und bestätigen Sie Ihre Adresse."
  | EmailConfirmationTitle -> "Bestätigung Ihrer Email Adresse"
  | EmtpyList field ->
    Format.asprintf
      "Es sind keine %s vorhanden."
      (Locales_de.field_to_string field)
  | ExperimentListTitle -> "Experimente"
  | ExperimentWaitingListTitle -> "Warteliste"
  | ExperimentContactEnrolledNote ->
    "Sie sind an der folgenden Session angemeldet:"
  | HomeTitle -> "Willkommen beim Pool Tool"
  | I18nTitle -> "Übersetzungen"
  | LocationListTitle -> "Standorte"
  | LocationNewTitle -> "Neuer Standort erstellen"
  | LocationNoSessions -> "Keine Sessions für diesen Standort gefunden."
  | LocationFileNew -> "Neue Datei zu Standort hinzufügen"
  | LoginTitle -> "Anmelden"
  | ResetPasswordLink | ResetPasswordTitle -> "Passwort zurücksetzen"
  | SessionDetailTitle start ->
    Format.asprintf "Session am %s" (Utils_time.formatted_date_time start)
  | SessionSignUpTitle -> "Für diese Session anmelden"
  | SignUpAcceptTermsAndConditions -> "Ich akzeptiere die Nutzungsbedingungen."
  | SignUpTitle -> "Registrieren"
  | TermsAndConditionsTitle -> "Nutzungsbedingungen"
  | UserProfileDetailsSubtitle -> "Persönliche Angaben"
  | UserProfileLoginSubtitle -> "Anmeldeinformationen"
  | UserProfilePausedNote ->
    "Sie haben alle Benachrichtigungen für Ihren Benutzer pausiert! (Klicken \
     Sie auf 'Bearbeiten', um diese Einstellung)"
  | UserProfileTitle -> "Benutzerprofil"
  | WaitingListIsDisabled -> "Die Warteliste ist deaktiviert."
;;

let nav_link_to_string = function
  | Assignments -> "Anmeldungen"
  | Dashboard -> "Dashboard"
  | Experiments -> "Experimente"
  | I18n -> "Übersetzungen"
  | Invitations -> "Einladungen"
  | Locations -> "Standorte"
  | LoginInformation -> "Anmeldeinformationen"
  | Logout -> "Logout"
  | Overview -> "Übersicht"
  | PersonalDetails -> "Persönliche Angaben"
  | Profile -> "Profil"
  | Sessions -> "Sessions"
  | Settings -> "Einstellungen"
  | Tenants -> "Tenants"
  | WaitingList -> "Warteliste"
;;

let hint_to_string = function
  | AssignContactFromWaitingList ->
    "Wählen Sie die Session, zu welcher Sie den Kontakt zuweisen wollen."
  | DirectRegistrationDisbled ->
    "Ist diese Option aktiviert, können sich Kontakte auf die Warteliste \
     setzen, aber nicht direkt für das Experiment einschreiben."
  | NumberIsDaysHint -> "Anzahl Tage"
  | NumberIsWeeksHint -> "Anzahl Wochen"
  | Overbook ->
    "Anzahl Probanden, die sich zusätzlich zur maximalen Anzahl Teilnehmer, an \
     einer Session einschreiben können."
  | RegistrationDisabled ->
    "Ist diese Option aktiviert, können sich Probanden weder anmelden noch auf \
     die Warteliste setzen. Das Experiment ist für die Kontakte nicht \
     ersichtlich."
  | SignUpForWaitingList ->
    "Das Rekrutierungsteam wird sich mit Ihnen in Verbindung setzen, um Ihnen \
     einen Termin zuzuweisen, wenn ein freier Platz vorhanden ist."
;;

let confirmable_to_string confirmable =
  (match confirmable with
  | CancelSession -> "die Session", "absagen"
  | DeleteEmailSuffix -> "das Suffix", "löschen"
  | DeleteExperiment -> "das Experiment", "löschen"
  | DeleteFile -> "die Datei", "löschen"
  | DeleteSession -> "die Session", "löschen")
  |> fun (obj, action) ->
  Format.asprintf "Sind Sie sicher, dass Sie %s %s wollen?" obj action
;;
