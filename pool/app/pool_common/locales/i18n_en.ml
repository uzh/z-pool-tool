open Entity_i18n

let to_string = function
  | DashboardTitle -> "Dashboard"
  | EmailConfirmationNote ->
    "Please check your emails and confirm your address first."
  | EmailConfirmationTitle -> "Email confirmation"
  | HomeTitle -> "Welcome to the Pool Tool"
  | I18nTitle -> "Translations"
  | LoginTitle -> "Login"
  | ResetPasswordLink | ResetPasswordTitle -> "Reset password"
  | SignUpAcceptTermsAndConditions -> "I accept the terms and conditions."
  | SignUpTitle -> "Sign up"
  | TermsAndConditionsTitle -> "Terms and Conditions"
  | UserProfileLoginSubtitle -> "Login information"
  | UserProfileTitle -> "User Profile"
  | UserProfilePausedNote ->
    "You paused all notifications for your user! (Click 'edit' to update this \
     setting)"
;;
