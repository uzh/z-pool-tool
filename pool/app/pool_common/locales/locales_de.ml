open Entity_message

let field_to_string = function
  | Admin -> "Administrator"
  | ContactEmail -> "Kontakt Email Adresse"
  | DatabaseLabel -> "Datenbanklabel"
  | DatabaseUrl -> "Datenbankurl"
  | Description -> "Beschreibung"
  | Email -> "Email Adresse"
  | EmailAddress -> "Email Adresse"
  | EmailSuffix -> "Email Endung"
  | FileMimeType -> "Mime Typ"
  | Filename -> "Dateiname"
  | Filesize -> "Dateigrösse"
  | Firstname -> "Vorname"
  | Host -> "Host"
  | Icon -> "Icon"
  | InactiveUserDisableAfter -> "Deaktiviere inaktiven Benutzer nach"
  | InactiveUserWarning -> "Warnung an inaktiven Benutzer"
  | Language -> "Sprache"
  | Lastname -> "Nachname"
  | LogoType -> "Logo Typ"
  | Operator -> "Operator"
  | Page -> "Seite"
  | Participant -> "Teilnehmer"
  | Password -> "Passwort"
  | Paused -> "Pausiert"
  | RecruitmentChannel -> "Rekrutierungs Kanal"
  | Role -> "Rolle"
  | Root -> "Root"
  | Setting -> "Einstellung"
  | SmtpAuthMethod -> "Smtp Authentifizierungsmethode"
  | SmtpAuthServer -> "Smtp Authentifizierungsserver"
  | SmtpPassword -> "Smtp Passwort"
  | SmtpPort -> "Smtp Port"
  | SmtpProtocol -> "Smtp Protokoll"
  | SmtpUsername -> "Smtp Benutzername"
  | Styles -> "Styles"
  | Tenant -> "Tenant"
  | TenantDisabledFlag -> "Deaktiviert Flag"
  | TenantLogos -> "Tenant Logos"
  | TenantMaintenanceFlag -> "Wartungsflag"
  | TenantPool -> "Tenant Pool"
  | TermsAndConditions -> "Teilnahmebedingungen"
  | TimeSpan -> "Zeitspanne"
  | Title -> "Titel"
  | Token -> "Token"
  | Url -> "Url"
  | User -> "Benutzer"
;;

let rec info_to_string : info -> string = function
  | InfoList infos -> list_msg_to_string info_to_string infos
  | TryPromoteOperator -> "Versuche den User zu Operator hochzustufen!"
;;

let rec success_to_string : success -> string = function
  | Created field ->
    field_message "" (field_to_string field) "wurde erfolgreich erstellt."
  | EmailVerified -> "Email erfolgreich verifiziert."
  | EmailConfirmationMessage ->
    "Eine Email wurde an deine Email Adresse zur verifizierung gesendet."
  | FileDeleted -> "File wurde erfolgreich gelöscht."
  | PasswordChanged -> "Passwort wurde geändert."
  | PasswordReset -> "Passwort ist zurückgesetzt, du kannst dich nun einloggen."
  | PasswordResetSuccessMessage ->
    "Falls ein Account zu der von dir eingegebenen Email Adresse existiert, \
     wird dir ein Email mit einem Link zur Passwort zurücksetzung gesendet."
  | SettingsUpdated -> "Die Einstellungen wurden erfolgreich gespeichert."
  | SuccessList successes -> list_msg_to_string success_to_string successes
  | TenantUpdateDatabase ->
    "Datenbank Informationen wurden erfolgreich upgedated."
  | TenantUpdateDetails -> "Tenant wurde erfolgreich upgedated."
  | Updated field ->
    field_message "" (field_to_string field) "wurde erfolgreich upgedated."
;;

let rec warning_to_string : warning -> string = function
  | WarningList warnings -> list_msg_to_string warning_to_string warnings
;;

let rec error_to_string = function
  | AdminAndParticipantSimul ->
    "Benutzer ist Admin und Teilnehmer gleichzeitig!"
  | AlreadyExists field ->
    field_message "Benuzer bereits" (field_to_string field) "!"
  | CantPromote -> "Kann nicht zu Operator hochgestuft werden!"
  | Conformist err -> ConformistError.to_string err
  | DecodeAction -> "Die Aktion konnte nicht gefunden werden."
  | EmailAddressMissingOperator -> "Bitte Operator Email Adresse angeben."
  | EmailAddressMissingRoot -> "Bitte Root Email Adresse angeben."
  | EmailAlreadyInUse -> "Email Adresse wird bereits verwendet."
  | EmailMalformed -> "Fehlerhafte Email Adresse"
  | ErrorList errors -> list_msg_to_string error_to_string errors
  | Invalid field ->
    field_message "Ungültige/r" (field_to_string field) "mitgeliefert!"
  | LoginProvideDetails -> "Bitte Email Adresse und Passwort eintragen."
  | MeantimeUpdate field ->
    field_message
      ""
      (field_to_string field)
      "wurde in der Zwischenzeit bearbeitet!"
  | NoOptionSelected field ->
    field_message "Bitte mindestens eine" (field_to_string field) "auswählen."
  | NoTenantsRegistered ->
    "Es sind keine Tenants auf der Root Datenbank registriert!"
  | NotFound field ->
    field_message "" (field_to_string field) "konnte nicht gefunden werden!"
  | ParticipantSignupInvalidEmail ->
    "Bitte eine valide und nicht bereits verwendete Email Adresse verwenden."
  | ParticipantUnconfirmed -> "Teilnehmer noch nicht verifiziert!"
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
  | TerminatoryTenantError | TerminatoryRootError ->
    "Bitte versuchen Sie es später erneut."
  | TerminatoryTenantErrorTitle | TerminatoryRootErrorTitle ->
    "Ein Fehler is aufgetreten."
  | TermsAndConditionsNotAccepted ->
    "Die Teilnahmebedingungen sind noch nicht akzeptiert."
  | TimeSpanPositive -> "Zeitspanne muss grösser als 0 sein!"
  | TokenInvalidFormat -> "Ungültiges Token Format!"
  | TokenAlreadyUsed -> "Das Token wurde bereits verwendet."
  | Undefined field ->
    field_message "" (field_to_string field) "ist undefiniert."
  | WriteOnlyModel -> "Model ausschliesslich zum auf die Datenbank schreiben!"
;;

let rec to_string = function
  | ErrorM error -> error_to_string error
  | InfoM info -> info_to_string info
  | MessageList msgs -> list_msg_to_string to_string msgs
  | Message string -> string
  | PageNotFoundMessage -> "Die Seite konnte nicht gefunden werden."
  | SuccessM success -> success_to_string success
  | WarningM warning -> warning_to_string warning
;;
