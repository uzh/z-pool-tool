open Entity_message

let field_to_string =
  let open Field in
  function
  | Admin -> "Administrator"
  | AdminHint -> "Hint für Administratoren"
  | Answer -> "Antwort"
  | AllowUninvitedSignup -> "Anmeldung nicht eingeladener Kontakte erlauben"
  | AssetId -> "Anlagen Identifier"
  | Assignment -> "Anmeldung"
  | AssignmentCount -> "Anz. Anmeldungen"
  | Assignments -> "Anmeldungen"
  | Building -> "Gebäude"
  | CanceledAt -> "Abgesagt am"
  | City -> "Ort"
  | Comment -> "Kommentar"
  | Contact -> "Proband"
  | ContactEmail -> "Kontakt Email Adresse"
  | Contacts -> "Probanden"
  | CustomField -> "Feld"
  | CreatedAt -> "Erstellt am"
  | CurrentPassword -> "Aktuelles Passwort"
  | CustomHtmx (label, _) -> label
  | Database -> "Datenbank"
  | DatabaseLabel -> "Datenbanklabel"
  | DatabaseUrl -> "Datenbankurl"
  | Date -> "Datum"
  | DateTime -> "Datum und Uhrzeit"
  | DefaultLanguage -> "Standard Sprache"
  | Description -> "Beschreibung"
  | DirectRegistrationDisabled -> "Direkte Registrierung deaktiviert"
  | Disabled -> "Gesperrt"
  | Distribution -> "Verteilung"
  | DistributionField -> "Feld"
  | Duration -> "Dauer"
  | Email -> "Email Adresse"
  | EmailAddress -> "Email Adresse"
  | EmailAddressUnverified -> "Unverifizierte Email Adresse"
  | EmailAddressVerified -> "Verifizierte Email Adresse"
  | EmailSuffix -> "Email Endung"
  | End -> "Ende"
  | Experiment -> "Experiment"
  | ExperimentType -> "Experimenttyp"
  | FieldType -> "Feldtyp"
  | File -> "Datei"
  | FileMapping -> "Datei zuweisung"
  | FileMimeType -> "Mime Typ"
  | Filename -> "Dateiname"
  | Filesize -> "Dateigrösse"
  | Firstname -> "Vorname"
  | FollowUpSession -> "Folgesession"
  | Hint -> "Hint"
  | Host -> "Host"
  | I18n -> "Übersetzung"
  | Icon -> "Icon"
  | Id -> "ID"
  | InactiveUserDisableAfter -> "Deaktiviere inaktiven Benutzer nach"
  | InactiveUserWarning -> "Warnung an inaktiven Benutzer"
  | Institution -> "Institution"
  | Invitation -> "Einladung"
  | InvitationSubject -> "Einladungsbetreff"
  | InvitationText -> "Einladungstext"
  | Invitations -> "Einladungen"
  | InvitationCount -> "Anz. Einladungen"
  | Key -> "Schlüssel"
  | Label -> "Label"
  | Language -> "Sprache"
  | LanguageDe -> "Deutsch"
  | LanguageEn -> "Englisch"
  | Lastname -> "Nachname"
  | LeadTime -> "Vorlaufzeit"
  | Link -> "Link"
  | Location -> "Lokalität"
  | LogoType -> "Logo Typ"
  | Mailing -> "Versand"
  | MainSession -> "Hauptsession"
  | MaxParticipants -> "Maximum an Teilnehmern"
  | MinParticipants -> "Minimum an Teilnehmern"
  | Model -> "Modell"
  | Name -> "Name"
  | NewPassword -> "Neues Passwort"
  | Order -> "Reihenfolge"
  | Operator -> "Operator"
  | Overbook -> "Überbuchen"
  | Overwrite -> "overwrite"
  | Page -> "Seite"
  | Participant | Participants -> "Teilnehmer"
  | ParticipantCount -> "Anzahl Teilnehmer"
  | Participated -> "teilgenommen"
  | PartnerLogos -> "Partner logos"
  | Password -> "Passwort"
  | PasswordConfirmation -> "Passwort wiederholen"
  | Paused -> "Pausiert"
  | Profile -> "Profil"
  | PublicTitle -> "Öffentlicher Titel"
  | Rate -> "Höchstrate"
  | RecruitmentChannel -> "Rekrutierungs Kanal"
  | RegistrationDisabled -> "Registrierung deaktiviert"
  | ReminderText -> "Erinnerungstext"
  | ReminderSubject -> "Erinnerungsbetreff"
  | Required -> "Benötigt"
  | ResentAt -> "Erneut verschickt"
  | Role -> "Rolle"
  | Room -> "Raum"
  | Root -> "Root"
  | Session -> "Session"
  | Sessions -> "Sessions"
  | Setting -> "Einstellung"
  | ShowUp -> "Anwesend"
  | SmtpAuthMethod -> "Smtp Authentifizierungsmethode"
  | SmtpAuthServer -> "Smtp Authentifizierungsserver"
  | SmtpPassword -> "Smtp Passwort"
  | SmtpPort -> "Smtp Port"
  | SmtpProtocol -> "Smtp Protokoll"
  | SmtpReadModel -> "Smtp read model"
  | SmtpUsername -> "Smtp Benutzername"
  | SmtpWriteModel -> "Smtp write model"
  | SortOrder -> "Sortierung"
  | Start -> "Start"
  | Status -> "Status"
  | Street -> "Strasse"
  | Styles -> "Styles"
  | Tenant -> "Tenant"
  | TenantDisabledFlag -> "Deaktiviert"
  | TenantId -> "Tenant Identifier"
  | TenantLogos -> "Tenant Logos"
  | TenantMaintenanceFlag -> "Wartungsflag"
  | TenantPool -> "Tenant Pool"
  | TermsAccepted -> "Akzeptieren"
  | TermsAndConditions -> "Teilnahmebedingungen"
  | Time -> "Uhrzeit"
  | TimeSpan -> "Zeitspanne"
  | Title -> "Titel"
  | Token -> "Token"
  | Translation -> "Übersetzung"
  | TriggerProfileUpdateAfter -> "Aufforderung zur Kontrolle des Profils"
  | Url -> "Url"
  | User -> "Benutzer"
  | Validation -> "Validierung"
  | Version -> "Version"
  | Virtual -> "Virtuell"
  | WaitingList -> "Warteliste"
  | Zip -> "PLZ"
;;

let info_to_string : info -> string = function
  | Info s -> s
  | RequiredFieldsMissing ->
    "Bitte beantworten Sie die folgenden Fragen um fortzufahren."
;;

let success_to_string : success -> string = function
  | AddedToWaitingList -> "Sie wurden der Warteliste hinzugefügt."
  | AssignmentCreated -> "Sie wurden erfolgreich angemeldet."
  | Canceled field ->
    field_message "" (field_to_string field) "wurde erfolgreich abgesagt."
  | Created field ->
    field_message "" (field_to_string field) "wurde erfolgreich erstellt."
  | Deleted field ->
    field_message "" (field_to_string field) "wurde erfolgreich gelöscht."
  | EmailConfirmationMessage ->
    "Eine Email wurde an deine Email Adresse zur verifizierung gesendet."
  | EmailVerified -> "Email erfolgreich verifiziert."
  | FileDeleted -> "File wurde erfolgreich gelöscht."
  | PasswordChanged -> "Passwort wurde geändert."
  | PasswordReset -> "Passwort ist zurückgesetzt, du kannst dich nun einloggen."
  | PasswordResetSuccessMessage ->
    "Falls ein Account zu der von dir eingegebenen Email Adresse existiert,  \
     wird dir ein Email mit einem Link zur Passwort zurücksetzung gesendet."
  | RemovedFromWaitingList -> "Sie wurden von der Warteliste entfernt."
  | Rescheduled field ->
    field_message "" (field_to_string field) "wurden erfolgreich verschoben."
  | SentList field ->
    field_message "" (field_to_string field) "wurden erfolgreich verschickt."
  | SettingsUpdated -> "Die Einstellungen wurden erfolgreich gespeichert."
  | Stopped field ->
    field_message "" (field_to_string field) "wurde erfolgreich gestoppt."
  | TenantUpdateDatabase ->
    "Datenbank Informationen wurden erfolgreich upgedated."
  | TenantUpdateDetails -> "Tenant wurde erfolgreich upgedated."
  | Updated field ->
    field_message "" (field_to_string field) "wurde erfolgreich upgedated."
;;

let warning_to_string : warning -> string = function
  | Warning string -> string
;;

let rec error_to_string = function
  | AllLanguagesRequired field ->
    field_message
      "Bitte geben Sie '"
      (field |> field_to_string |> CCString.trim)
      "' in allen Sprachen an."
  | AlreadyInPast ->
    "Mindestens der Startzeitpunkt liegt bereits in der Vergangenheit."
  | AlreadySignedUpForExperiment ->
    "Sie haben sich für dieses Experiment bereits angemeldet."
  | AlreadyStarted ->
    "Bereits gestarted oder beendet, aktion nicht mehr möglich."
  | AlreadyInvitedToExperiment names ->
    Format.asprintf
      "Die folgenden Kontakte wurden bereits zu diesem Experiment eingeladen: \
       %s"
      (CCString.concat ", " names)
  | Authorization message ->
    field_message "Autorisierung nicht möglich: " message ""
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
  | ContactSignupInvalidEmail ->
    "Bitte eine valide und nicht bereits verwendete Email Adresse verwenden."
  | ContactUnconfirmed -> "Teilnehmer noch nicht verifiziert!"
  | Decode field ->
    field_message
      ""
      (field_to_string field)
      "konnte nicht entschlüsselt werden."
  | DecodeAction -> "Die Aktion konnte nicht gefunden werden."
  | DirectRegistrationIsDisabled ->
    "Sie können sich nicht selbst für dieses Experiment anmelden."
  | Disabled field ->
    field_message "" (field_to_string field) "ist deaktiviert."
  | EmailAddressMissingOperator -> "Bitte Operator Email Adresse angeben."
  | EmailAddressMissingRoot -> "Bitte Root Email Adresse angeben."
  | EmailAlreadyInUse -> "Email Adresse wird bereits verwendet."
  | EmailDeleteAlreadyVerified ->
    "Email Adresse ist bereits verifiziert, kann nicht gelöscht werden."
  | EmailMalformed -> "Fehlerhafte Email Adresse"
  | EndBeforeStart -> "Das Ende liegt vor oder dem Start."
  | ExperimentSessionCountNotZero ->
    "Es existieren Sessions zu diesem Experiment. Es kann nicht gelöscht  \
     werden."
  | FollowUpIsEarlierThanMain ->
    "Folgesession kann nicht vor Hauptsession starten."
  | HtmxVersionNotFound field ->
    Format.asprintf "Version von '%s' konnte nicht gefunden werden." field
  | Invalid field -> field_message "" (field_to_string field) "ist ungültig!"
  | InvalidHtmxRequest -> "Ungültige Anfrage."
  | InvitationSubjectAndTextRequired ->
    "Bitte geben Sie sowohl den Betreff als auch den Text der Einladung an."
  | LoginProvideDetails -> "Bitte Email Adresse und Passwort eintragen."
  | MeantimeUpdate field ->
    field_message
      ""
      (field_to_string field)
      "wurde in der Zwischenzeit bearbeitet!"
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
  | PasswordConfirmationDoesNotMatch ->
    "Passwortbestätigung stimmt nicht mit dem neuen Passwort überein."
  | PasswordPolicy -> "Passwort stimmt nicht mit der benötigten Policy überein!"
  | PasswordResetFailMessage ->
    "Falls ein Account zu der von dir eingegebenen Email Adresse existiert,  \
     wird dir ein Email mit einem Link zur Passwort zurücksetzung gesendet."
  | PasswordResetInvalidData -> "Ungültiges Token oder Passwort."
  | PoolContextNotFound -> "Kontext konnte nicht gefunden werden."
  | ReadOnlyModel -> "Model ausschliesslich um von der Datenbank zu lesen!"
  | RegistrationDisabled -> "Registrierung ist deaktiviert."
  | RequestRequiredFields -> "Bitte alle notwendigen Felder ausfüllen."
  | Retrieve field ->
    field_message "" (field_to_string field) "konnte nicht gefunden werden."
  | SessionHasAssignments ->
    "Es existieren bereits Anmeldungen für diese Session. Sie kann nicht \
     gelöscht werden."
  | SessionFullyBooked -> "Session ist ausgebucht"
  | SessionInvalid -> "Ungültige Session, bitte erneut einloggen."
  | ReminderSubjectAndTextRequired ->
    "Bitte geben Sie sowohl den Betreff als auch den Text für die Session \
     Erinnerung an."
  | SessionTenantNotFound ->
    "Auf unserer Seite ist etwas schief gegangen, bitte später nochmals  \
     versuchen. Falls der Fehler mehrmals auftritt, bitte den Adminstrator  \
     kontaktieren."
  | Smaller (field1, field2) ->
    Format.asprintf
      "%s kleiner als %s"
      (field_to_string field1)
      (field_to_string field2)
  | TerminatoryTenantError | TerminatoryRootError ->
    "Bitte versuchen Sie es später erneut."
  | TerminatoryTenantErrorTitle | TerminatoryRootErrorTitle ->
    "Ein Fehler is aufgetreten."
  | TermsAndConditionsMissing ->
    "Die Teilnamhebedingungen müssen zuerst erfasst werden."
  | TermsAndConditionsNotAccepted ->
    "Die Teilnahmebedingungen sind noch nicht akzeptiert."
  | TextLengthMax i -> Format.asprintf "Darf nicht länger als %i sein." i
  | TextLengthMin i -> Format.asprintf "Darf nicht kürzer als %i sein." i
  | TimeInPast -> "Zeitpunkt liegt in der Vergangenheint!"
  | TimeSpanPositive -> "Zeitspanne muss grösser als 0 sein!"
  | TokenAlreadyUsed -> "Das Token wurde bereits verwendet."
  | TokenInvalidFormat -> "Ungültiges Token Format!"
  | Undefined field ->
    field_message "" (field_to_string field) "ist undefiniert."
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
  | Assign field -> format_submit "zuweisen" field
  | Back -> format_submit "zurück" None
  | Cancel field -> format_submit "absagen" field
  | Choose field -> format_submit "wählen" field
  | Create field -> format_submit "erstellen" field
  | Decline -> format_submit "ablehnen" None
  | Delete field -> format_submit "löschen" field
  | Descending -> "absteigend"
  | Disable -> format_submit "deaktivieren" None
  | Edit field -> format_submit "bearbeiten" field
  | Enable -> format_submit "aktivieren" None
  | Enroll -> format_submit "einschreiben" None
  | Login -> format_submit "anmelden" None
  | More -> "mehr"
  | PleaseSelect -> "bitte wählen"
  | RemoveFromWaitingList -> "Ich möchte mich von der Warteliste austragen"
  | Reschedule field -> format_submit "verschieben" field
  | Resend field -> format_submit "erneut senden" field
  | Save field -> format_submit "speichern" field
  | SelectFilePlaceholder -> format_submit "datei auswählen.." None
  | Send field -> format_submit "senden" field
  | SendResetLink -> format_submit "link senden" None
  | Show -> "anzeigen"
  | SignUp -> format_submit "registrieren" None
  | Stop field -> format_submit "stoppen" field
  | Update field -> format_submit "aktualisieren" field
;;

let to_string = function
  | Message string -> string
  | PageNotFoundMessage -> "Die Seite konnte nicht gefunden werden."
;;
