open Entity_message

let field_to_string = function
  | Admin -> "Administrator"
  | ContactEmail -> "Kontakt Email Adresse"
  | CurrentPassword -> "Aktuelles Passwort"
  | DatabaseLabel -> "Datenbanklabel"
  | DatabaseUrl -> "Datenbankurl"
  | DefaultLanguage -> "Standard Sprache"
  | Description -> "Beschreibung"
  | Email -> "Email Adresse"
  | EmailAddress -> "Email Adresse"
  | EmailSuffix -> "Email Endung"
  | FileMimeType -> "Mime Typ"
  | Filename -> "Dateiname"
  | Filesize -> "Dateigrösse"
  | Firstname -> "Vorname"
  | Host -> "Host"
  | I18n -> "Übersetzung"
  | Icon -> "Icon"
  | InactiveUserDisableAfter -> "Deaktiviere inaktiven Benutzer nach"
  | InactiveUserWarning -> "Warnung an inaktiven Benutzer"
  | Key -> "Schlüssel"
  | Language -> "Sprache"
  | Lastname -> "Nachname"
  | LogoType -> "Logo Typ"
  | NewPassword -> "Neues Passwort"
  | Operator -> "Operator"
  | Page -> "Seite"
  | Participant -> "Teilnehmer"
  | Password -> "Passwort"
  | PasswordConfirmation -> "Passwort wiederholen"
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
  | Translation -> "Übersetzung"
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
  | PasswordChanged -> "Passwort wurde geändert."
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
  | HtmxVersionNotFound field ->
    Format.asprintf "Version von '%s' konnte nicht gefunden werden." field
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
  | SessionTenantNotFound ->
    "Auf unserer Seite ist etwas schief gegangen, bitte später nochmals \
     versuchen. Falls der Fehler mehrmals auftritt, bitte den Adminstrator \
     kontaktieren."
  | TerminatoryTenantError | TerminatoryRootError ->
    "Bitte versuchen Sie es später erneut."
  | TerminatoryTenantErrorTitle | TerminatoryRootErrorTitle ->
    "Ein Fehler is aufgetreten."
  | TermsAndConditionsMissing ->
    "Die Teilnamhebedingungen müssen zuerst erfasst werden."
  | TermsAndConditionsNotAccepted ->
    "Die Teilnahmebedingungen sind noch nicht akzeptiert."
  | TimeSpanPositive -> "Zeitspanne muss grösser als 0 sein!"
  | TokenInvalidFormat -> "Ungültiges Token Format!"
  | TokenAlreadyUsed -> "Das Token wurde bereits verwendet."
  | Undefined field ->
    field_message "" (field_to_string field) "ist undefiniert."
  | WriteOnlyModel -> "Model ausschliesslich zum auf die Datenbank schreiben!"
;;

let format_submit submit field =
  let field_opt_message f =
    f |> CCOption.map field_to_string |> Option.value ~default:""
  in
  field_message (field_opt_message field) submit ""
;;

let submit_to_string = function
  | Accept field -> format_submit "akzeptieren" field
  | Add field -> format_submit "hinzufügen" field
  | Back -> format_submit "zurück" None
  | Choose field -> format_submit "wählen" field
  | Create field -> format_submit "erstellen" field
  | Delete field -> format_submit "löschen" field
  | Decline -> format_submit "ablehnen" None
  | Disable -> format_submit "deaktivieren" None
  | Edit field -> format_submit "bearbeiten" field
  | Enable -> format_submit "aktivieren" None
  | Login -> format_submit "anmelden" None
  | Save field -> format_submit "speichern" field
  | SendResetLink -> format_submit "link senden" None
  | SignUp -> format_submit "registrieren" None
  | Update field -> format_submit "aktualisieren" field
;;

let to_string = function
  | Message string -> string
  | PageNotFoundMessage -> "Die Seite konnte nicht gefunden werden."
;;
