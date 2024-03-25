open Entity_message

let rec field_to_string =
  let combine one two =
    CCString.concat ": " [ field_to_string one; field_to_string two ]
  in
  let open Field in
  function
  | Action -> "Aktion"
  | Actor -> "Akteur"
  | ActiveContactsCount -> "Anzahl aktive Kontakte"
  | Admin -> "Administrator"
  | AdminComment -> "Adminkommentar"
  | AdminInput -> "Admineingabe"
  | AdminHint -> "Hint für Administratoren"
  | AdminInputOnly -> "Eingabe nur durch Admins"
  | AdminViewOnly -> "Nur für Admins ersichtlich"
  | AllowUninvitedSignup -> "Einschreiben aller Kontakte erlauben"
  | Answer -> "Antwort"
  | AreaCode -> "Vorwahl"
  | Argument -> "Argument"
  | AssetId -> "Anlagen Identifier"
  | AssignableRole -> "zuweisbare Rolle"
  | Assignment -> "Anmeldung"
  | AssignmentCount -> "Anmeldungen"
  | Assignments -> "Anmeldungen"
  | AssignmentsCreated -> "Sessionanmeldungen"
  | Assistants -> "Assistenten"
  | AvailableLanguages -> "Verfügbare Sprachen"
  | Building -> "Gebäude"
  | CanceledAt -> "Abgesagt am"
  | CellPhone -> "Mobiltelefon"
  | Chronological -> "chronologisch"
  | City -> "Ort"
  | ClosedAt -> "Geschlossen am"
  | ConfirmedAt -> "Bestätigt am"
  | Contact -> "Kontakt"
  | ContactCount -> "Anzahl Kontakte"
  | ContactEmail -> "Kontakt E-Mail-Adresse"
  | ContactLanguage -> "Kontakt- & Anzeigesprache"
  | ContactPerson -> "Kontaktperson"
  | Contacts -> "Kontakte"
  | CostCenter -> "Kostenstelle"
  | Count -> "Anzahl"
  | Context -> "Kontext"
  | CreatedAt -> "Erstellt am"
  | CurrentPassword -> "Aktuelles Passwort"
  | CustomField -> "Feld"
  | CustomFieldGroup -> "Gruppe"
  | CustomFieldGroups -> "Gruppen"
  | CustomFieldOption -> "Option"
  | CustomFieldOptions -> "Optionen"
  | CustomFields -> "Felder"
  | CustomHtmx (label, _) -> label
  | Database -> "Datenbank"
  | DatabaseLabel -> "Datenbanklabel"
  | DatabaseUrl -> "Datenbankurl"
  | Date -> "Datum"
  | DateTime -> "Datum und Uhrzeit"
  | DefaultLanguage -> "Standard Sprache"
  | DefaultSmtpServer -> "Standardserver"
  | Description -> "Beschreibung"
  | DirectRegistrationDisabled -> "Direkte Registrierung deaktiviert"
  | Disabled -> "Gesperrt"
  | Distribution -> "Verteilung"
  | DistributionField -> "Feld"
  | Duration -> "Dauer"
  | Email -> "E-Mail"
  | EmailAddress -> "E-Mail-Adresse"
  | EmailAddressUnverified -> "Unverifizierte E-Mail-Adresse"
  | EmailAddressVerified -> "Verifizierte E-Mail-Adresse"
  | EmailLeadTime -> "Email Vorlaufzeit"
  | EmailRemindersSentAt -> "Email Erinnerungen verschickt am"
  | EmailSubject -> "E-Mail Betreff"
  | EmailSuffix -> "E-Mail Endung"
  | EmailText -> "E-Mail Text"
  | EmailsSent -> "Insgesamt gesendete E-Mails"
  | End -> "Ende"
  | Exclude -> "Ausgenommen"
  | ExcludeRolesOf -> "Ausgenommen (Rollen von jmd)"
  | Experiment -> "Experiment"
  | ExperimentCount -> "Anz. Experimente"
  | Experiments -> "Experimente"
  | Experimenter -> "Experimenter"
  | ExperimentEmailReminderLeadTime ->
    Format.asprintf
      "Experimentspezifische Erinnerungsemail %s"
      (field_to_string LeadTime)
  | ExperimentTextMessageReminderLeadTime ->
    Format.asprintf
      "Experimentspezifische Erinnerungs-SMS %s"
      (field_to_string LeadTime)
  | ExperimentType -> "Experimenttyp"
  | ExternalDataIdAbbr -> "EID"
  | ExternalDataId -> "Externer Daten Identifikator"
  | ExternalDataRequired -> "Externe Daten müssen angegeben werden"
  | Failed -> "Fehlgeschlagen"
  | FallbackToEmail ->
    "Möchten Sie die Nachricht als E-Mail an Kontakte ohne Mobiltelefon senden?"
  | FieldType -> "Feldtyp"
  | File -> "Datei"
  | FileMapping -> "Datei zuweisung"
  | FileMimeType -> "Mime Typ"
  | Filename -> "Dateiname"
  | Filesize -> "Dateigrösse"
  | Filter -> "Filter"
  | Firstname -> "Vorname"
  | FirstReminder -> "Erste Erinnerung"
  | FollowUpSession -> "Folgesession"
  | GtxApiKey -> "GTX Api Key"
  | HideCanceled -> "Abgesagte ausblenden"
  | HideClosed -> "Geschlossene ausblenden"
  | HideMakedAsDeleted -> "Als gelöscht markerte ausblenden"
  | HidePaused -> "Pausierte ausblenden"
  | HideInactive -> "Inaktive ausblenden"
  | HidePast -> "Vergangene ausblenden"
  | HideUnverified -> "Unverifizierte ausblenden"
  | Hint -> "Hint"
  | Host -> "Host"
  | I18n -> "Übersetzung"
  | Icon -> "Icon"
  | Id -> "ID"
  | ImportPending -> "Import pendent"
  | InactiveUserDisableAfter -> "Deaktiviere inaktiven Benutzer nach"
  | InactiveUserWarning -> "Warnung an inaktiven Benutzer"
  | Inactive -> "Inaktiv"
  | Input -> "Input"
  | Institution -> "Institution"
  | InternalDescription -> "Interne Beschreibung"
  | Interval -> "Interval"
  | Invitation -> "Einladung"
  | InvitationCount -> "Anz. Einladungen"
  | InvitationResetAt -> "Einladungen zurückgesetzt am"
  | Invitations -> "Einladungen"
  | InvitationsSent -> "Gesendete Einladungen"
  | InvitationSubject -> "Einladungsbetreff"
  | InvitationText -> "Einladungstext"
  | Key -> "Schlüssel"
  | Label -> "Label"
  | Language -> "Sprache"
  | LanguageDe -> "Deutsch"
  | LanguageEn -> "Englisch"
  | LastError -> "Letzte Fehlernachricht"
  | LastErrorAt -> "Letzter Fehler"
  | LastManuallyRemindedAt -> "Zuletzt manuell erinnert am"
  | Lastname -> "Nachname"
  | LastRemindedAt -> "Erinnert am"
  | LastRun -> "Letzter Durchlauf"
  | LastRunAt -> "Letzter Durchlauf"
  | LeadTime -> "Vorlaufzeit"
  | Limit -> "Limit"
  | Link -> "Link"
  | Location -> "Standort"
  | Locations -> "Standorte"
  | LoginCount -> "Logins von Kontakten"
  | LogoType -> "Logo Typ"
  | Mailing -> "Versand"
  | MainSession -> "Hauptsession"
  | MarkedAsDeleted -> "Als gelöscht markiert"
  | MaxParticipants -> "Maximum an Teilnehmern"
  | MaxTries -> "Maximum an Versuchen"
  | Message -> "Nachricht"
  | MessageChannel -> "Nachrichtenkanal"
  | MessageTemplate -> "Nachrichtentemplate"
  | MessageTemplates -> "Nachrichtentemplates"
  | MinParticipants -> "Minimum an Teilnehmern"
  | Model -> "Modell"
  | Name -> "Name"
  | NewPassword -> "Neues Passwort"
  | NextRunAt -> "Nächster Versuch um"
  | NoShow -> "Nicht anwesend"
  | NoShowAbr -> "NS"
  | NoShowCount -> "Abwesende"
  | NotMatchingFilterCount -> "unpassende"
  | NotifiedAt -> "Benachrichtigt am"
  | NotifyVia -> "Benachrichtigen via"
  | NotifyContact -> "Kontakt benachrichtigen"
  | Offset -> "Offset"
  | Operator -> "Operator"
  | Operators -> "Operatoren"
  | Order -> "Reihenfolge"
  | OrganisationalUnit -> "Organisationseinheit"
  | Overbook -> "Überbuchen"
  | OverriddenValue -> "Überschriebene Kontakt-Antwort"
  | Override -> "Überschreiben"
  | Page -> "Seite"
  | PageCount -> "Anzahl Seiten"
  | Participant | Participants -> "Teilnehmer"
  | ParticipantCount -> "Teilnehmer"
  | Participated -> "teilgenommen"
  | ParticipatedAbr -> "P"
  | ParticipationTag -> "Teilnahmetag"
  | PartnerLogos -> "Partner logos"
  | Password -> "Passwort"
  | PasswordConfirmation -> "Passwort wiederholen"
  | Paused -> "Pausiert"
  | PendingContactImports -> "Pendente Kontaktimporte"
  | Period -> "Zeitraum"
  | Permission -> "Berechtigung"
  | PlainText -> "Klartext"
  | Predicate -> "Prädikat"
  | Profile -> "Profil"
  | PublicDescription -> "Öffentliche Beschreibung"
  | PromptOnRegistration -> "Eingabeaufforderung bei der Registrierung"
  | PublicTitle -> "Öffentlicher Titel"
  | PublishedAt -> "Veröffentlicht"
  | Query -> "Query"
  | Queue -> "Warteschlange"
  | RandomOrder -> "Wähle die Kontakte in zufälliger Reihenfolge."
  | Reason -> "Grund"
  | Recipient -> "Empfänger"
  | Redirect -> "Weiterleitung"
  | RegistrationDisabled -> "Registrierung deaktiviert"
  | RegistrationPossible -> "Registrierung möglich"
  | Reminder -> "Erinnerung"
  | ReminderCount -> "Anzahl Reminder"
  | RemindersSent -> "Gesendete reminders"
  | Required -> "Benötigt"
  | ResentAt -> "Erneut verschickt"
  | Role -> "Rolle"
  | Room -> "Raum"
  | Root -> "Root"
  | Rule -> "Regel"
  | ScheduledTime -> "Geplante Zeit"
  | ScheduledTimeSpan -> "Wiederholungs Interval"
  | Search -> "Suche"
  | SearchOf field -> combine Search field
  | SecondReminder -> "Zweite Erinnerung"
  | Sender -> "Absender"
  | SendingInvitations -> "Verschickt Einladungen"
  | SentAt -> "Verschickt am"
  | SessionCount -> "Anz. Sessions"
  | Session -> "Session"
  | Sessions -> "Sessions"
  | Setting -> "Einstellung"
  | Settings -> "Einstellungen"
  | ShowUpCount -> "Anwesende"
  | ShowExteralDataIdLinks -> "Link zu externen Datenidentifikatoren anzeigen"
  | SignedUpAt -> "Eingeschrieben am"
  | SignUpCount -> "Neuregistrierungen"
  | SMS -> "SMS"
  | SmsText -> "SMS Text"
  | Smtp -> "SMTP"
  | SmtpLabel -> "Label"
  | SmtpMechanism -> "Authentifizierungsmechanismus"
  | SmtpPassword -> "Passwort"
  | SmtpPort -> "Port"
  | SmtpProtocol -> "Protokoll"
  | SmtpServer -> "Server"
  | SmtpUsername -> "Benutzername"
  | SortOrder -> "Sortierung"
  | Start -> "Start"
  | StartNow -> "Jetzt starten"
  | Status -> "Status"
  | Street -> "Strasse"
  | Styles -> "Styles"
  | Successful -> "Erfolgreich"
  | SystemEvent -> "System Event"
  | Tag -> "Tag"
  | Tags -> "Tags"
  | Tagging -> "Tagging"
  | Target -> "Target"
  | Template -> "Template"
  | Tenant -> "Tenant"
  | TenantDisabledFlag -> "Deaktiviert"
  | TenantId -> "Tenant Identifier"
  | TenantLogos -> "Tenant Logos"
  | TenantMaintenanceFlag -> "Wartungsflag"
  | TenantPool -> "Tenant Pool"
  | TermsAccepted -> "Akzeptieren"
  | TermsAcceptedCount -> "Teilnahmebedingungen akzeptiert"
  | TermsAndConditions -> "Teilnahmebedingungen"
  | TestPhoneNumber -> "Testtelefonnummer"
  | TextMessage -> "SMS"
  | TextMessageLeadTime -> "SMS Vorlaufzeit"
  | TextMessageRemindersSentAt -> "SMS Erinnerungen verschickt am"
  | Time -> "Uhrzeit"
  | TimeSpan -> "Zeitspanne"
  | TimeUnit -> "Zeiteinheit"
  | TimeUnitOf field -> combine TimeUnit field
  | Title -> "Titel"
  | ToHandle -> "zu bearbeiten"
  | Token -> "Token"
  | Total -> "Total"
  | Translation -> "Übersetzung"
  | Tries -> "Versuche"
  | TriggerProfileUpdateAfter -> "Aufforderung zur Kontrolle des Profils"
  | Url -> "Url"
  | User -> "Benutzer"
  | Validation -> "Validierung"
  | Value -> "Wert"
  | ValueOf field -> combine Value field
  | Verified -> "Verifiziert"
  | Version -> "Version"
  | Virtual -> "Virtuell"
  | WaitingList -> "Warteliste"
  | Year -> "Jahr"
  | Zip -> "PLZ"
;;

let info_to_string : info -> string = function
  | Info s -> s
;;

let success_to_string : success -> string = function
  | AddedToWaitingList -> "Sie wurden der Warteliste hinzugefügt."
  | AssignmentCreated -> "Sie wurden erfolgreich angemeldet."
  | Canceled field ->
    field_message "" (field_to_string field) "wurde erfolgreich abgesagt."
  | Closed field ->
    field_message "" (field_to_string field) "wurde erfolgreich geschlossen."
  | ContactPromoted -> "Der Kontakt wurde erfolgreich zum Admin befördert."
  | Created field ->
    field_message "" (field_to_string field) "wurde erfolgreich erstellt."
  | Deleted field ->
    field_message "" (field_to_string field) "wurde erfolgreich gelöscht."
  | EmailConfirmationMessage ->
    "Eine E-Mail wurde an deine E-Mail-Adresse zur Verifizierung gesendet, \
     falls die angegebene E-Mail-Adresse verfügbar ist."
  | EmailUpdateConfirmationMessage ->
    {|Falls die angegebene E-Mail-Adresse verfügbar ist, wurde eine E-Mail mit einem Bestätigungslink an diese Adresse geschickt. Bitte bestätige die Adresse mit dem Öffnen dieses Links.

Solange die neue E-Mail-Adresse nicht bestätigt ist, wird weiterhin die aktuelle Adresse verwendet.|}
  | EmailVerified -> "E-Mail erfolgreich verifiziert."
  | FileDeleted -> "File wurde erfolgreich gelöscht."
  | ImportCompleted ->
    "Der Import Ihres Kontos wurde erfolgreich abgeschlossen."
  | MarkedAsDeleted field ->
    field_message "" (field_to_string field) "wurde als gelöscht markiert."
  | PausedToggled paused ->
    Format.asprintf
      "Der Account wurde erfolgreich %s."
      (if paused then "pausiert" else "reaktiviert")
  | PasswordChanged -> "Passwort wurde geändert."
  | PasswordReset -> "Passwort ist zurückgesetzt, du kannst dich nun einloggen."
  | PasswordResetSuccessMessage ->
    "Falls ein Account zu der von dir eingegebenen E-Mail-Adresse existiert,  \
     wird dir ein E-Mail mit einem Link zur Passwort zurücksetzung gesendet."
  | CellPhoneTokenSent ->
    "Es wurde eine Textnachricht zur Überprüfung an Ihr Telefon gesendet. \
     Bitte geben Sie den angegebenen Code ein."
  | CellPhoneVerified -> "Ihre Telefonnummer wurde erfolgreich geprüft."
  | Published field ->
    field_message "" (field_to_string field) "wurde erfolgreich veröffentlicht."
  | RemovedFromWaitingList -> "Sie wurden von der Warteliste entfernt."
  | RemindersResent -> "Die Erinnerungen wirden erneut versendet."
  | Rescheduled field ->
    field_message "" (field_to_string field) "wurden erfolgreich verschoben."
  | Resent field ->
    field_message "" (field_to_string field) "wurden erneut verschickt."
  | ResetInvitations ->
    "Einladungsversand zurückgesetzt. Bei kommenden versanden werden frühere \
     Einladungen ignoriert respektive die Einladung erneut versendet."
  | RoleAssigned -> "Rolle wurde zugewiesen."
  | RoleUnassigned -> "Rolle wurde entzogen."
  | SentList field ->
    field_message "" (field_to_string field) "wurden erfolgreich verschickt."
  | Sent field ->
    field_message "" (field_to_string field) "wurde erfolgreich verschickt."
  | SettingsUpdated -> "Die Einstellungen wurden erfolgreich gespeichert."
  | SmtpConfigurationAdded ->
    "Die SMTP Konfiguration wurde erfolgreich hinzugefügt."
  | SmtpDetailsUpdated ->
    "Die SMTP Einstellungen wurden erfolgreich gespeichert."
  | SmtpPasswordUpdated -> "Das SMTP Passwort wurde erfolgreich gespeichert."
  | Stopped field ->
    field_message "" (field_to_string field) "wurde erfolgreich gestoppt."
  | TagAssigned -> "Der Tag wurde hinzugefügt."
  | TagRemoved -> "Der Tag wurde entfernt."
  | TenantUpdateDatabase ->
    "Datenbank Informationen wurden erfolgreich upgedated."
  | TenantUpdateDetails -> "Tenant wurde erfolgreich upgedated."
  | Updated field ->
    field_message "" (field_to_string field) "wurde erfolgreich upgedated."
  | Validated field ->
    field_message "" (field_to_string field) "wurde erfolgreich validiert."
  | VerificationMessageResent ->
    "Die Verifizierungsnachricht wurde erneut verschickt."
;;

let warning_to_string : warning -> string = function
  | Warning string -> string
;;

let rec error_to_string = function
  | AccountTemporarilySuspended ptime ->
    ptime
    |> Utils.Ptime.formatted_date_time
    |> Format.asprintf
         "Zu viele fehlgeschlagene Anmeldeversuche. Diese E-Mail-Adresse ist \
          gesperrt, bis %s"
  | AccessDenied -> "Zugriff verweigert"
  | AccessDeniedMessage ->
    "Der Zugriff auf die gewünschte Seite ist nicht möglich."
  | AllLanguagesRequired field ->
    field_message
      "Bitte geben Sie '"
      (field |> field_to_string |> CCString.trim)
      "' in allen Sprachen an."
  | AlreadyExisting field ->
    field_message
      "Die Daten zum Feld '"
      (field |> field_to_string |> CCString.trim)
      "' existieren bereits."
  | AlreadyInPast ->
    "Mindestens der Startzeitpunkt liegt bereits in der Vergangenheit."
  | AlreadySignedUpForExperiment ->
    "Sie haben sich für dieses Experiment bereits angemeldet."
  | AlreadyPublished field ->
    field_message
      ""
      (field |> field_to_string |> CCString.trim)
      "wurde bereits veröffentlich."
  | AssignmentIsCanceled -> "Anmeldung wurde abgesagt."
  | AssignmentIsClosed -> "Anmeldung wurde bereits geschlossen."
  | AssignmentsHaveErrors ->
    "Einige Anmeldungen haben Fehler. Bitte korrigieren Sie diese zuerst."
  | AlreadyStarted ->
    "Bereits gestarted oder beendet, aktion nicht mehr möglich."
  | AlreadyInvitedToExperiment names ->
    Format.asprintf
      "Die folgenden Kontakte wurden bereits zu diesem Experiment eingeladen: \
       %s"
      (CCString.concat ", " names)
  | Authorization message ->
    field_message "Autorisierung nicht möglich: " message ""
  | CannotBeDeleted field ->
    Format.asprintf "%s kann nicht gelöscht werden." (field_to_string field)
  | CannotBeUpdated field ->
    Format.asprintf "%s kann nicht angepasst werden." (field_to_string field)
  | Conformist errs ->
    CCList.map
      (fun (field, err) ->
        Format.asprintf
          "%s: %s"
          (field_to_string field |> CCString.capitalize_ascii)
          (error_to_string err))
      errs
    |> CCString.concat "\n"
  | ConformistModuleErrorType -> failwith "Do not use"
  | ContactDoesNotMatchFilter ->
    "Der Kontakt erfüllt die im Filter bestimmten Kriterien nicht."
  | ContactExperimentNotFound ->
    "Derzeit gibt es keine freien Plätze für die Teilnahme an diesem \
     Experiment."
  | ContactIsInactive -> "Dieser Kontakt ist inaktiv."
  | ContactSignupInvalidEmail ->
    "Bitte eine valide und nicht bereits verwendete E-Mail-Adresse verwenden."
  | ContactUnconfirmed -> "Teilnehmer noch nicht verifiziert!"
  | CustomFieldNoOptions -> "Es muss mindestens eine Option existieren."
  | CustomFieldTypeChangeNotAllowed ->
    "Sie können den Typ des Feldes nicht ändern."
  | Decode field ->
    field_message
      ""
      (field_to_string field)
      "konnte nicht entschlüsselt werden."
  | DecodeAction -> "Die Aktion konnte nicht gefunden werden."
  | DefaultMustNotBeUnchecked -> "'Standard' kann nicht deaktiviert werden."
  | DirectRegistrationIsDisabled ->
    "Sie können sich nicht selbst für dieses Experiment anmelden."
  | Disabled field ->
    field_message "" (field_to_string field) "ist deaktiviert."
  | EmailAddressMissingAdmin -> "Bitte Admin E-Mail-Adresse angeben."
  | EmailAddressMissingRoot -> "Bitte Root E-Mail-Adresse angeben."
  | EmailAlreadyInUse -> "E-Mail-Adresse wird bereits verwendet."
  | EmailDeleteAlreadyVerified ->
    "E-Mail-Adresse ist bereits verifiziert, kann nicht gelöscht werden."
  | EmailIdenticalToCurrent ->
    "Die angegebene E-Mail-Adresse ist identisch mit der aktuellen."
  | EmailMalformed -> "Fehlerhafte E-Mail-Adresse"
  | EmailInterceptionError error ->
    Format.asprintf "Email interception error: %s" error
  | EndBeforeStart -> "Das Ende liegt vor oder dem Start."
  | ExperimentSessionCountNotZero ->
    "Es existieren Sessions zu diesem Experiment. Es kann nicht gelöscht  \
     werden."
  | FieldRequired field ->
    Format.asprintf
      "%s wird benötigt."
      (field_to_string field |> CCString.capitalize_ascii)
  | FilterMustNotContainTemplate -> "Filter darf keine Template enthalten."
  | FilterAndOrMustNotBeEmpty ->
    "'And' und 'Or' Prädikate dürfen nicht leer sein."
  | FilterListValueMustNotBeEmpty ->
    "Es muss mindestens eine Option angewählt werden."
  | FollowUpIsEarlierThanMain ->
    "Folgesession kann nicht vor Hauptsession starten."
  | HtmxVersionNotFound field ->
    Format.asprintf "Version von '%s' konnte nicht gefunden werden." field
  | ImportPending ->
    "Der Import Ihres Users ist noch nicht abgeschlossen. Bitte kontrollieren \
     Sie Ihren Posteingang oder kontaktieren Sie einen Administrator."
  | Invalid field -> field_message "" (field_to_string field) "ist ungültig!"
  | InvalidEmailSuffix suffixes ->
    Format.asprintf
      "%s Die folgenden E-Mail-Endungen sind erlaubt: %s"
      (error_to_string (Invalid Field.EmailSuffix))
      (CCString.concat ", " suffixes)
  | InvalidJson exn -> Format.asprintf "Ungültiges Json: %s" exn
  | InvalidOptionSelected -> "Ungültige Option ausgewählt."
  | InvalidRequest | InvalidHtmxRequest -> "Ungültige Anfrage."
  | IsMarkedAsDeleted field ->
    field_message
      ""
      (field |> field_to_string |> CCString.trim)
      "wurde als gelöscht markiert."
  | JobPending -> "Der Auftrag ist noch pendent."
  | LoginProvideDetails -> "Bitte Email Adresse und Passwort eintragen."
  | MeantimeUpdate field ->
    field_message
      ""
      (field_to_string field)
      "wurde in der Zwischenzeit bearbeitet!"
  | Missing field ->
    field_message
      "Das Feld"
      (field_to_string field)
      "fehlt oder ist nicht ausgefüllt."
  | MutuallyExclusive (f1, f2) ->
    Format.asprintf
      "%s und %s schliessen sich gegenseitig aus."
      (field_to_string f1)
      (field_to_string f2)
  | NegativeAmount -> "Hat negative Anzahl!"
  | NoOptionSelected field ->
    field_message "Bitte mindestens eine" (field_to_string field) "auswählen."
  | NotADatetime (time, err) ->
    Format.asprintf "%s: '%s' ist kein valides Datum." err time
  | NotANumber field -> Format.asprintf "'%s' ist keine Nummer." field
  | NoTenantsRegistered ->
    "Es sind keine Tenants auf der Root Datenbank registriert!"
  | NotEligible -> "Sie sind nicht befugt, diese Aktuion durchzuführen."
  | NotFound field ->
    field_message "" (field_to_string field) "konnte nicht gefunden werden!"
  | NotFoundList (field, items) ->
    field_message
      "Folgende"
      (field_to_string field)
      (Format.asprintf
         "konnten nicht gefunden werden: %s"
         (CCString.concat "," items))
  | NotHandled field ->
    Format.asprintf "Feld '%s' wird nicht verarbeitet." field
  | NotInTimeRange -> "Nicht im angegebenen Zeitfenster."
  | NoValue -> "Kein Wert angegeben"
  | NumberMax i -> Format.asprintf "Darf nicht grösser als %i sein." i
  | NumberMin i -> Format.asprintf "Darf nicht kleiner als %i sein." i
  | Or (err1, err2) ->
    CCFormat.asprintf
      "%s oder %s"
      (error_to_string err1)
      (err2 |> error_to_string |> CCString.uncapitalize_ascii)
  | PasswordConfirmationDoesNotMatch ->
    "Passwortbestätigung stimmt nicht mit dem neuen Passwort überein."
  | PasswordPolicyMinLength n ->
    Format.asprintf "Das Passwort muss mindestens %i Zeichen lang sein." n
  | PasswordPolicyCapitalLetter ->
    "Das Passwort muss einen Grossbuchstaben enthalten."
  | PasswordPolicyNumber -> "Das Passwort muss eine Zahl enthalten."
  | PasswordPolicySpecialChar chars ->
    Format.asprintf
      "Das Passwort muss eines der folgenden Zeichen enthalten: %s"
      (chars |> CCList.map CCString.of_char |> CCString.concat " ")
  | PasswordResetFailMessage ->
    "Falls ein Account zu der von dir eingegebenen E-Mail-Adresse existiert,  \
     wird dir ein E-Mail mit einem Link zur Passwort zurücksetzung gesendet."
  | PasswordResetInvalidData -> "Ungültiges Token oder Passwort."
  | PermissionDeniedCreateRule ->
    "Berechtigung zur Erstellung der Regel verweigert."
  | PermissionDeniedGrantRole -> "Berechtigung der Rollenzuweisung verweigert."
  | PermissionDeniedRevokeRole ->
    "Berechtigung für den Widerruf der Rolle verweigert."
  | PickMessageChannel ->
    "Kein Nachrichtenkanal wurde ausgewählt für die Benachrichtigung der \
     Kontakte."
  | PoolContextNotFound -> "Kontext konnte nicht gefunden werden."
  | QueryNotCompatible (f1, f2) ->
    Format.asprintf
      "%s ist nicht kompatibel mit %s."
      (field_to_string f1)
      (field_to_string f2)
  | ReadOnlyModel -> "Model ausschliesslich um von der Datenbank zu lesen!"
  | RegistrationDisabled -> "Registrierung ist deaktiviert."
  | RequestRequiredFields -> "Bitte alle notwendigen Felder ausfüllen."
  | RequiredFieldsMissing ->
    "Bitte beantworten Sie die folgenden Fragen um fortzufahren."
  | Retrieve field ->
    field_message "" (field_to_string field) "konnte nicht gefunden werden."
  | SessionFullyBooked -> "Session ist ausgebucht"
  | SessionHasAssignments ->
    "Es existieren bereits Anmeldungen für diese Session. Sie kann nicht \
     gelöscht werden."
  | SessionHasFollowUps ->
    "Es existieren bereits Folgesessions für diese Session. Sie kann nicht \
     gelöscht werden."
  | SessionInvalid -> "Ungültige Session, bitte erneut einloggen."
  | SelectedOptionsCountMax i ->
    let verb, noun =
      if i == 1 then "darf", "Option" else "dürfen", "Optionen"
    in
    Format.asprintf "Es %s höchstens %i %s ausgewählt werden." verb i noun
  | SelectedOptionsCountMin i ->
    let verb, noun =
      if i == 1 then "muss", "Option" else "müssen", "Optionen"
    in
    Format.asprintf "Es %s mindestens %i %s ausgewählt werden." verb i noun
  | SessionRegistrationViaParent -> "Einschreibung via Hauptsession."
  | SessionTenantNotFound ->
    "Auf unserer Seite ist etwas schief gegangen, bitte später nochmals  \
     versuchen. Falls der Fehler mehrmals auftritt, bitte den Adminstrator  \
     kontaktieren."
  | SessionAlreadyCanceled date ->
    CCFormat.asprintf "Diese Session wurde bereits abgesagt am %s." date
  | SessionAlreadyClosed date ->
    CCFormat.asprintf "Diese Session wurde bereits geschlossen am %s." date
  | SessionNotClosed -> "Diese Session wurde noch nicht geschlossen."
  | SessionInPast -> "Diese Session ist beendet."
  | SessionNotStarted -> "Diese Session kann noch nicht geschlossen werden."
  | Smaller (field1, field2) ->
    Format.asprintf
      "%s kleiner als %s"
      (field_to_string field1)
      (field_to_string field2)
  | SmtpException exn -> exn
  | TerminatoryTenantError | TerminatoryRootError ->
    "Bitte versuchen Sie es später erneut."
  | TerminatoryTenantErrorTitle | TerminatoryRootErrorTitle ->
    "Ein Fehler is aufgetreten."
  | TermsAndConditionsMissing ->
    "Die Teilnamhebedingungen müssen zuerst erfasst werden."
  | TermsAndConditionsNotAccepted ->
    "Die Teilnahmebedingungen sind noch nicht akzeptiert."
  | TextLengthMax i ->
    Format.asprintf "Darf nicht länger als %i Zeichen sein." i
  | TextLengthMin i ->
    Format.asprintf "Darf nicht kürzer als %i Zeichen sein." i
  | TextMessageInterceptionError error ->
    Format.asprintf "Text message interception error: %s" error
  | TimeInPast -> "Zeitpunkt liegt in der Vergangenheint!"
  | TimeSpanPositive -> "Zeitspanne muss grösser als 0 sein!"
  | TokenAlreadyUsed -> "Das Token wurde bereits verwendet."
  | TokenInvalidFormat -> "Ungültiges Token Format!"
  | TooShort -> "Die angegebene Dauer ist zu kurz."
  | Undefined field ->
    field_message "" (field_to_string field) "ist undefiniert."
  | Uniqueness field ->
    field_message "" (field_to_string field) "muss einzigartig sein."
  | WriteOnlyModel -> "Model ausschliesslich zum auf die Datenbank schreiben!"
;;

let format_submit submit field =
  let field_opt_message f =
    f |> CCOption.map field_to_string |> CCOption.value ~default:""
  in
  field_message (field_opt_message field) submit ""
;;

let control_to_string = function
  | Accept field -> format_submit "akzeptieren" field
  | Add field -> format_submit "hinzufügen" field
  | AddToWaitingList -> "Ich möchte mich zur Warteliste hinzufügen"
  | Ascending -> "aufsteigend"
  | Apply -> "anwenden"
  | Assign field -> format_submit "zuweisen" field
  | Back -> format_submit "zurück" None
  | Cancel field -> format_submit "absagen" field
  | ChangeSession -> format_submit "ändern" (Some Entity_message_field.Session)
  | Choose field -> format_submit "wählen" field
  | Close field -> format_submit "schliessen" field
  | Create field -> format_submit "erstellen" field
  | Decline -> format_submit "ablehnen" None
  | Delete field -> format_submit "löschen" field
  | Descending -> "absteigend"
  | Disable -> format_submit "deaktivieren" None
  | Duplicate field -> format_submit "duplizieren" field
  | Edit field -> format_submit "bearbeiten" field
  | Enable -> format_submit "aktivieren" None
  | Enroll -> format_submit "einschreiben" None
  | EnterNewCellPhone -> "eine andere Nummer eingeben"
  | Filter field -> format_submit "filtern" field
  | Login -> format_submit "login" None
  | LoadDefaultTemplate -> format_submit "Standardtemplate laden" None
  | Manage field -> format_submit "manage" (Some field)
  | MarkAsDeleted -> format_submit "als gelöscht markieren" None
  | More -> "mehr"
  | NextPage -> "weiter"
  | OpenProfile -> "Profil anzeigen"
  | PauseAccount -> "Account pausieren"
  | PleaseSelect -> "bitte wählen"
  | PreviousPage -> "zurück"
  | PromoteContact -> "Kontakt befördern"
  | Print field -> format_submit "drucken" field
  | PublicPage -> "Öffne öffentliche Seite"
  | Publish field -> format_submit "veröffentlichen" field
  | ReactivateAccount -> "Account reaktivieren"
  | Register -> format_submit "einschreiben" None
  | RemoveFromWaitingList -> "Ich möchte mich von der Warteliste austragen"
  | Remove field -> format_submit "entfernen" field
  | Reschedule field -> format_submit "verschieben" field
  | ResetPlainText ->
    Format.asprintf
      "%s zu formatiertem '%s' zurücksetzen"
      (field_to_string Field.PlainText)
      (field_to_string Field.EmailText)
  | Resend field -> format_submit "erneut senden" field
  | Reset field -> format_submit "zurücksetzen" field
  | ResetForm -> "Formular zurücksetzen"
  | Save field -> format_submit "speichern" field
  | SessionDetails -> format_submit "Sessiondetails" None
  | Select -> format_submit "auswählen" None
  | SelectAll field -> format_submit "alle auswählen" field
  | SelectFilePlaceholder -> format_submit "datei auswählen.." None
  | Send field -> format_submit "senden" field
  | SendResetLink -> format_submit "link senden" None
  | Show -> "anzeigen"
  | SignUp -> format_submit "anmelden" None
  | Stop field -> format_submit "stoppen" field
  | ToggleAll -> "alle umschalten"
  | Unassign field -> format_submit "entfernen" field
  | Update field -> format_submit "aktualisieren" field
  | UpdateAssignmentsMatchFilter -> format_submit "Filter erneut ausführen" None
  | UpdateOrder -> "Reihenfolge anpassen"
  | Validate -> "Validieren"
  | Verify field -> format_submit "verifizieren" field
;;

let to_string = function
  | Message string -> string
  | PageNotFoundMessage -> "Die Seite konnte nicht gefunden werden."
;;
