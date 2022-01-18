open Entity_i18n

let to_string = function
  | EmailConfirmationNote ->
    "Please check your emails and confirm your address first."
  | EmailConfirmationTitle -> "Email confirmation"
  | LoginTitle -> "Login"
  | ResetPasswordLink -> "Reset password"
  | SignUpAcceptTermsAndConditions -> "I accept the terms and conditions."
  | SignUpTitle -> "Sign up"
;;
