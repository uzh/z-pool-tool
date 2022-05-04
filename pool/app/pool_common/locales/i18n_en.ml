open Entity_i18n

let to_string = function
  | DashboardTitle -> "Dashboard"
  | EmailConfirmationNote ->
    "Please check your emails and confirm your address first."
  | EmailConfirmationTitle -> "Email confirmation"
  | ExperimentNewTitle -> "Create new experiment"
  | ExperimentListTitle -> "Experiments"
  | ExperimentWaitingListTitle -> "Waiting list"
  | HomeTitle -> "Welcome to the Pool Tool"
  | I18nTitle -> "Translations"
  | InvitationListTitle -> "Invitations"
  | InvitationNewTitle -> "Send invitation"
  | LoginTitle -> "Login"
  | ResetPasswordLink | ResetPasswordTitle -> "Reset password"
  | SignUpAcceptTermsAndConditions -> "I accept the terms and conditions."
  | SignUpTitle -> "Sign up"
  | TermsAndConditionsTitle -> "Terms and Conditions"
  | UserProfileLoginSubtitle -> "Login information"
  | UserProfileDetailsSubtitle -> "Personal details"
  | UserProfileTitle -> "User Profile"
  | UserProfilePausedNote ->
    "You paused all notifications for your user! (Click 'edit' to update this \
     setting)"
;;

let nav_link_to_string = function
  | Dashboard -> "Dashboard"
  | Experiments -> "Experiments"
  | I18n -> "Translations"
  | Profile -> "Profile"
  | Invitations -> "Invitations"
  | Settings -> "Settings"
  | Tenants -> "Tenants"
  | WaitingList -> "Waiting list"
;;
