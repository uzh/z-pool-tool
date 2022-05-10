open Entity_i18n

let to_string = function
  | DashboardTitle -> "Dashboard"
  | EmailConfirmationNote ->
    "Please check your emails and confirm your address first."
  | EmailConfirmationTitle -> "Email confirmation"
  | ExperimentListTitle -> "Experiments"
  | ExperimentNewTitle -> "Create new experiment"
  | ExperimentWaitingListTitle -> "Waiting list"
  | HomeTitle -> "Welcome to the Pool Tool"
  | I18nTitle -> "Translations"
  | InvitationListTitle -> "Invitations"
  | InvitationNewTitle -> "Send invitation"
  | LoginTitle -> "Login"
  | ResetPasswordLink | ResetPasswordTitle -> "Reset password"
  | SessionListTitle -> "Sessions"
  | SessionNewTitle -> "New Session"
  | SessionUpdateTitle -> "Update Session"
  | SignUpAcceptTermsAndConditions -> "I accept the terms and conditions."
  | SignUpTitle -> "Sign up"
  | TermsAndConditionsTitle -> "Terms and Conditions"
  | UserProfileDetailsSubtitle -> "Personal details"
  | UserProfileLoginSubtitle -> "Login information"
  | UserProfilePausedNote ->
    "You paused all notifications for your user! (Click 'edit' to update this  \
     setting)"
  | UserProfileTitle -> "User Profile"
;;

let nav_link_to_string = function
  | Dashboard -> "Dashboard"
  | Experiments -> "Experiments"
  | I18n -> "Translations"
  | Invitations -> "Invitations"
  | Profile -> "Profile"
  | Sessions -> "Sessions"
  | Settings -> "Settings"
  | Tenants -> "Tenants"
  | WaitingList -> "Waiting list"
;;
