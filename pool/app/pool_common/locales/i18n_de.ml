open Entity_i18n

let to_string = function
  | DashboardTitle -> "Dashboard"
  | EmailConfirmationNote ->
    "Bitte prüfen Sie zunächst Ihre E-Mails und bestätigen Sie Ihre Adresse."
  | EmailConfirmationTitle -> "Bestätigung Ihrer Email Adresse"
  | ExperimentNewTitle -> "Neues Experiment erstellen"
  | ExperimentListTitle -> "Experimente"
  | HomeTitle -> "Willkommen beim Pool Tool"
  | I18nTitle -> "Übersetzungen"
  | LoginTitle -> "Anmelden"
  | ResetPasswordLink | ResetPasswordTitle -> "Passwort zurücksetzen"
  | SignUpAcceptTermsAndConditions -> "Ich akzeptiere die Nutzungsbedingungen."
  | SignUpTitle -> "Registrieren"
  | TermsAndConditionsTitle -> "Nutzungsbedingungen"
  | UserProfileLoginSubtitle -> "Anmeldeinformationen"
  | UserProfileDetailsSubtitle -> "Persönliche Angaben"
  | UserProfileTitle -> "Benutzerprofil"
  | UserProfilePausedNote ->
    "Sie haben alle Benachrichtigungen für Ihren Benutzer pausiert! (Klicken \
     Sie auf 'Bearbeiten', um diese Einstellung)"
;;

let nav_link_to_string = function
  | Dashboard -> "Dashboard"
  | Experiments -> "Experimente"
  | I18n -> "Übersetzungen"
  | Profile -> "Profil"
  | Settings -> "Einstellungen"
  | Tenants -> "Tenants"
;;
