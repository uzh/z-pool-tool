open Entity_message

let field_to_string = function
  | Admin -> "Administrator"
  | ChangeSetVersion -> "ChangeSet Version"
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
  | TenantMaintenanceFlag -> "Wartungsflag"
  | TermsAndConditions -> "Teilnahmebedingungen"
  | TimeSpan -> "Zeitspanne"
  | Title -> "Titel"
  | Token -> "Token"
  | Url -> "Url"
  | User -> "Benutzer"
;;

let info_to_string : info -> string = function
  | Info string -> string
;;

let success_to_string : success -> string = function
  | Created field ->
    field_message "" (field_to_string field) "wurde erfolgreich erstellt."
  | EmailVerified -> "Email erfolgreich verifiziert."
  | EmailConfirmationMessage ->
    "Eine Email wurde an deine Email Adresse zur verifizierung gesendet."
  | FileDeleted -> "File wurde erfolgreich gelöscht."
  | PasswordReset -> "Passwort ist zurückgesetzt, du kannst dich nun einloggen."
  | PasswordResetSuccessMessage ->
    "Falls ein Account zu der von dir eingegebenen Email Adresse existiert, \
     wird dir ein Email mit einem Link zur Passwort zurücksetzung gesendet."
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

let error_to_string = function
  | Conformist err -> ConformistError.to_string err
  | DecodeAction -> "Die Aktion konnte nicht gefunden werden."
  | EmailAddressMissingOperator -> "Bitte Operator Email Adresse angeben."
  | EmailAddressMissingRoot -> "Bitte Root Email Adresse angeben."
  | EmailAlreadyInUse -> "Email Adresse wird bereits verwendet."
  | EmailMalformed -> "Fehlerhafte Email Adresse"
  | Invalid field ->
    field_message "Ungültige/r" (field_to_string field) "mitgeliefert!"
  | LoginProvideDetails -> "Bitte Email Adresse und Passwort eintragen."
  | MeantimeUpdate field ->
    field_message
      ""
      (field_to_string field)
      "wurde in der Zwischenzeit bearbeitet!"
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
  | SessionTenantNotFound ->
    "Auf unserer Seite ist etwas schief gegangen, bitte später nochmals \
     versuchen. Falls der Fehler mehrmals auftritt, bitte den Adminstrator \
     kontaktieren."
  | TermsAndConditionsNotAccepted ->
    "Die Teilnahmebedingungen sind noch nicht akzeptiert."
  | TimeSpanPositive -> "Zeitspanne muss grösser als 0 sein!"
  | TokenInvalidFormat -> "Ungültiges Token Format!"
  | TokenAlreadyUsed -> "Das Token wurde bereits verwendet."
  | Undefined field ->
    field_message "" (field_to_string field) "ist undefiniert."
  | WriteOnlyModel -> "Model ausschliesslich zum auf die Datenbank schreiben!"
;;

let to_string = function
  | Message string -> string
  | PageNotFoundMessage -> "Die Seite konnte nicht gefunden werden."
;;
