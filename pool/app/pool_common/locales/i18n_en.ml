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
  | Files -> "Files"
  | HomeTitle -> "Welcome to the Pool Tool"
  | I18nTitle -> "Translations"
  | LocationListTitle -> "Location"
  | LocationNewTitle -> "Create new location"
  | LocationNoFiles -> "There are no files for this location."
  | LocationNoSessions -> "No sessions found for this location."
  | LocationFileNew -> "Add file to location"
  | LoginTitle -> "Login"
  | NumberIsDaysHint -> "Days"
  | NumberIsWeeksHint -> "Weeks"
  | RateDependencyHint ->
    "There are other mailings running at the same time. In case the sum of all \
     rates reaches the maximum of the server, they will automatically get \
     reduced."
  | RateHint -> "Generated Invitations per hour"
  | RateNumberPerMinutesHint number ->
    Format.asprintf "Generates new invitations every %d minutes." number
  | RateTotalSent -> "Totally generated invitations:"
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
  | LoginInformation -> "Login information"
  | Locations -> "Locations"
  | Logout -> "Logout"
  | Overview -> "Overview"
  | PersonalDetails -> "Personal details"
  | Profile -> "Profile"
  | Sessions -> "Sessions"
  | Settings -> "Settings"
  | Tenants -> "Tenants"
  | WaitingList -> "Waiting list"
;;

let hint_to_string = function
  | AssignContactFromWaitingList ->
    "Select the session to which you want to assign the contact."
  | DirectRegistrationDisbled ->
    "If this option is enabled, contacts can join the waiting list but cannot \
     directly enroll in the experiment."
  | NumberIsSecondsHint -> "Nr. of seconds"
  | NumberIsDaysHint -> "Nr. of days"
  | NumberIsWeeksHint -> "Nr. of weeks"
  | Overbook ->
    "Number of subjects that can enroll in a session in addition to the \
     maximum number of contacts."
  | RegistrationDisabled ->
    "If this option is activated, contacts can neither register nor join the \
     waiting list. The experiment is not visible to the contacts."
  | SignUpForWaitingList ->
    "The recruitment team will contact you, to assign you to a session, if \
     there is a free place."
;;

let confirmable_to_string confirmable =
  (match confirmable with
  | CancelSession -> "session", "cancel"
  | DeleteEmailSuffix -> "email suffix", "delete"
  | DeleteExperiment -> "experiment", "delete"
  | DeleteFile -> "the file", "delete"
  | DeleteSession -> "session", "delete")
  |> fun (obj, action) ->
  Format.asprintf "Are you sure you want to %s the %s?" action obj
;;
