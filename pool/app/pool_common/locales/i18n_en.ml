open Entity_i18n

let to_string = function
  | DashboardTitle -> "Dashboard"
  | EmailConfirmationNote ->
    "Please check your emails and confirm your address first."
  | EmailConfirmationTitle -> "Email confirmation"
  | EmtpyList field ->
    Format.asprintf
      "There are no %s available."
      (Locales_en.field_to_string field)
  | ExperimentListTitle -> "Experiments"
  | ExperimentWaitingListTitle -> "Waiting list"
  | ExperimentContactEnrolledNote -> "You signed up for the following session:"
  | HomeTitle -> "Welcome to the Pool Tool"
  | I18nTitle -> "Translations"
  | LocationListTitle -> "Location"
  | LocationNewTitle -> "Create new location"
  | LocationNoSessions -> "No sessions found for this location."
  | LocationFileNew -> "Add file to location"
  | LoginTitle -> "Login"
  | NumberIsDaysHint -> "Days"
  | NumberIsWeeksHint -> "Weeks"
  | ResetPasswordLink | ResetPasswordTitle -> "Reset password"
  | SessionDetailTitle start ->
    Format.asprintf "Session at %s" (Utils_time.formatted_date_time start)
  | SessionSignUpTitle -> "Sign up for this session"
  | SignUpAcceptTermsAndConditions -> "I accept the terms and conditions."
  | SignUpTitle -> "Sign up"
  | TermsAndConditionsTitle -> "Terms and Conditions"
  | UserProfileDetailsSubtitle -> "Personal details"
  | UserProfileLoginSubtitle -> "Login information"
  | UserProfilePausedNote ->
    "You paused all notifications for your user! (Click 'edit' to update this  \
     setting)"
  | UserProfileTitle -> "User Profile"
  | WaitingListIsDisabled -> "The waiting list is disabled."
;;

let nav_link_to_string = function
  | Assignments -> "Assignments"
  | Dashboard -> "Dashboard"
  | Experiments -> "Experiments"
  | I18n -> "Translations"
  | Invitations -> "Invitations"
  | Locations -> "Locations"
  | Overview -> "Overview"
  | Profile -> "Profile"
  | Sessions -> "Sessions"
  | Settings -> "Settings"
  | Tenants -> "Tenants"
  | WaitingList -> "Waiting list"
;;
