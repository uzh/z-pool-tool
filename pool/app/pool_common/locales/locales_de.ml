open Entity_message

let field_to_string =
  let open Field in
  function
  | Admin -> "Administrator"
  | AssetId -> "Anlagen Identifier"
  | CanceledAt -> "Abgesagt am"
  | ContactEmail -> "Kontakt Email Adresse"
  | CreatedAt -> "Erstellt am"
  | CurrentPassword -> "Aktuelles Passwort"
  | Database -> "Datenbank"
  | DatabaseLabel -> "Datenbanklabel"
  | DatabaseUrl -> "Datenbankurl"
  | Date -> "Datum"
  | DateTime -> "Datum und Uhrzeit"
  | DefaultLanguage -> "Standard Sprache"
  | Description -> "Beschreibung"
  | Disabled -> "Gesperrt"
  | Duration -> "Dauer"
  | Email -> "Email Adresse"
  | EmailAddress -> "Email Adresse"
  | EmailAddressUnverified -> "Unverifizierte Email Adresse"
  | EmailAddressVerified -> "Verifizierte Email Adresse"
  | EmailSuffix -> "Email Endung"
  | Experiment -> "Experiment"
  | File -> "Datei"
  | FileMimeType -> "Mime Typ"
  | Filename -> "Dateiname"
  | Filesize -> "Dateigrösse"
  | Firstname -> "Vorname"
  | Host -> "Host"
  | I18n -> "Übersetzung"
  | Icon -> "Icon"
  | Id -> "ID"
  | InactiveUserDisableAfter -> "Deaktiviere inaktiven Benutzer nach"
  | InactiveUserWarning -> "Warnung an inaktiven Benutzer"
  | Invitation -> "Einladung"
  | Invitations -> "Einladungen"
  | Key -> "Schlüssel"
  | Language -> "Sprache"
  | LanguageDe -> "Deutsch"
  | LanguageEn -> "Englisch"
  | Lastname -> "Nachname"
  | LogoType -> "Logo Typ"
  | MaxParticipants -> "Maximum an Teilnehmern"
  | MinParticipants -> "Minimum an Teilnehmern"
  | NewPassword -> "Neues Passwort"
  | Operator -> "Operator"
  | Overbook -> "Überbuchen"
  | Page -> "Seite"
  | Participant | Participants -> "Teilnehmer"
  | ParticipantCount -> "Anzahl Teilnehmer"
  | Participated -> "teilgenommen"
  | PartnerLogos -> "Partner logos"
  | Password -> "Passwort"
  | PasswordConfirmation -> "Passwort wiederholen"
  | Paused -> "Pausiert"
  | RecruitmentChannel -> "Rekrutierungs Kanal"
  | ResentAt -> "Erneut verschickt"
  | Role -> "Rolle"
  | Root -> "Root"
  | Session -> "Session"
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
  | Start -> "Start"
  | Styles -> "Styles"
  | Subject -> "Proband"
  | Subjects -> "Probanden"
  | Tenant -> "Tenant"
  | TenantDisabledFlag -> "Deaktiviert Flag"
  | TenantId -> "Tenant Identifier"
  | TenantLogos -> "Tenant Logos"
  | TenantMaintenanceFlag -> "Wartungsflag"
  | TenantPool -> "Tenant Pool"
  | TermsAccepted -> "Teilnahmebedingungen akzeptiert"
  | TermsAndConditions -> "Teilnahmebedingungen"
  | Time -> "Uhrzeit"
  | TimeSpan -> "Zeitspanne"
  | Title -> "Titel"
  | Token -> "Token"
  | Translation -> "Übersetzung"
  | Url -> "Url"
  | User -> "Benutzer"
  | Version -> "Version"
  | WaitingList -> "Warteliste"
;;

let info_to_string : info -> string = function
  | Info string -> string
;;

let success_to_string : success -> string = function
  | AddedToWaitingList -> "Sie wurden der Warteliste hinzugefügt."
  | Canceled field ->
    field_message "" (field_to_string field) "wurde erfolgreich abgesagt."
  | Created field ->
    field_message "" (field_to_string field) "wurde erfolgreich erstellt."
  | Deleted field ->
    field_message "" (field_to_string field) "wurde erfolgreich gelöscht."
  | EmailVerified -> "Email erfolgreich verifiziert."
  | EmailConfirmationMessage ->
    "Eine Email wurde an deine Email Adresse zur verifizierung gesendet."
  | FileDeleted -> "File wurde erfolgreich gelöscht."
  | PasswordChanged -> "Passwort wurde geändert."
  | PasswordReset -> "Passwort ist zurückgesetzt, du kannst dich nun einloggen."
  | PasswordResetSuccessMessage ->
    "Falls ein Account zu der von dir eingegebenen Email Adresse existiert, \
     wird dir ein Email mit einem Link zur Passwort zurücksetzung gesendet."
  | RemovedFromWaitingList -> "Sie wurden von der Warteliste entfernt."
  | SentList field ->
    field_message "" (field_to_string field) "wurden erfolgreich verschickt."
  | SettingsUpdated -> "Die Einstellungen wurden erfolgreich gespeichert."
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
  | DecodeAction -> "Die Aktion konnte nicht gefunden werden."
  | Decode field ->
    field_message
      ""
      (field_to_string field)
      "konnte nicht entschlüsselt werden."
  | Disabled field ->
    field_message "" (field_to_string field) "ist deaktiviert."
  | EmailAddressMissingOperator -> "Bitte Operator Email Adresse angeben."
  | EmailAddressMissingRoot -> "Bitte Root Email Adresse angeben."
  | EmailAlreadyInUse -> "Email Adresse wird bereits verwendet."
  | EmailDeleteAlreadyVerified ->
    "Email Adresse ist bereits verifiziert, kann nicht gelöscht werden."
  | EmailMalformed -> "Fehlerhafte Email Adresse"
  | ExperimentSessionCountNotZero ->
    "Es existieren Sessions zu diesem Experiment. Es kann nicht gelöscht \
     werden."
  | HtmxVersionNotFound field ->
    Format.asprintf "Version von '%s' konnte nicht gefunden werden." field
  | Invalid field -> field_message "" (field_to_string field) "ist ungültig!"
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
  | NoValue -> "Kein Wert angegeben"
  | SubjectSignupInvalidEmail ->
    "Bitte eine valide und nicht bereits verwendete Email Adresse verwenden."
  | SubjectUnconfirmed -> "Teilnehmer noch nicht verifiziert!"
  | PasswordConfirmationDoesNotMatch ->
    "Passwortbestätigung stimmt nicht mit dem neuen Passwort überein."
  | PasswordPolicy msg ->
    Format.asprintf
      "Passwort stimmt nicht mit der benötigten Policy überein! %s"
      msg
  | PasswordResetInvalidData -> "Ungültiges Token oder Passwort."
  | PasswordResetFailMessage ->
    "Falls ein Account zu der von dir eingegebenen Email Adresse existiert, \
     wird dir ein Email mit einem Link zur Passwort zurücksetzung gesendet."
  | RequestRequiredFields -> "Bitte alle notwendigen Felder ausfüllen."
  | Retrieve field ->
    field_message "" (field_to_string field) "konnte nicht gefunden werden."
  | SessionInvalid -> "Ungültige Session, bitte erneut einloggen."
  | SessionTenantNotFound ->
    "Auf unserer Seite ist etwas schief gegangen, bitte später nochmals \
     versuchen. Falls der Fehler mehrmals auftritt, bitte den Adminstrator \
     kontaktieren."
  | Smaller (field1, field2) ->
    Format.asprintf
      "%s kleiner als %s"
      (field_to_string field1)
      (field_to_string field2)
  | PoolContextNotFound -> "Kontext konnte nicht gefunden werden."
  | TerminatoryTenantError | TerminatoryRootError ->
    "Bitte versuchen Sie es später erneut."
  | TerminatoryTenantErrorTitle | TerminatoryRootErrorTitle ->
    "Ein Fehler is aufgetreten."
  | TermsAndConditionsMissing ->
    "Die Teilnamhebedingungen müssen zuerst erfasst werden."
  | TermsAndConditionsNotAccepted ->
    "Die Teilnahmebedingungen sind noch nicht akzeptiert."
  | TimeInPast -> "Zeitpunkt liegt in der Vergangenheint!"
  | TimeSpanPositive -> "Zeitspanne muss grösser als 0 sein!"
  | TokenInvalidFormat -> "Ungültiges Token Format!"
  | TokenAlreadyUsed -> "Das Token wurde bereits verwendet."
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
  | Back -> format_submit "zurück" None
  | Cancel field -> format_submit "absagen" field
  | Choose field -> format_submit "wählen" field
  | Create field -> format_submit "erstellen" field
  | Delete field -> format_submit "löschen" field
  | Decline -> format_submit "ablehnen" None
  | Disable -> format_submit "deaktivieren" None
  | Edit field -> format_submit "bearbeiten" field
  | Enable -> format_submit "aktivieren" None
  | Login -> format_submit "anmelden" None
  | More -> "mehr"
  | Save field -> format_submit "speichern" field
  | Resend field -> format_submit "erneut senden" field
  | RemoveFromWaitingList -> "Ich möchte mich von der Warteliste austragen"
  | Send field -> format_submit "senden" field
  | SendResetLink -> format_submit "link senden" None
  | SignUp -> format_submit "registrieren" None
  | Update field -> format_submit "aktualisieren" field
;;

let to_string = function
  | Message string -> string
  | PageNotFoundMessage -> "Die Seite konnte nicht gefunden werden."
;;
