open Entity_i18n

let to_string = function
  | DashboardTitle -> "Dashboard"
  | DontHaveAnAccount -> "Don't have an account?"
  | EmailConfirmationNote ->
    "Please check your emails and confirm your address first."
  | EmailConfirmationTitle -> "Email confirmation"
  | EmtpyList field ->
    Format.asprintf
      "There are no %s available."
      (Locales_en.field_to_string field)
  | ExperimentContactEnrolledNote -> "You signed up for the following session:"
  | ExperimentNewTitle -> "Create new experiment"
  | ExperimentListTitle -> "Experiments"
  | ExperimentWaitingListTitle -> "Waiting list"
  | ExperimentSessionReminderHint ->
    "These are default settings for the sessions of this experiment. These \
     settings can be overwritten for each session."
  | Files -> "Files"
  | FilterNrOfContacts ->
    "Number of contacts meeting the criteria of this filter:"
  | FollowUpSessionFor -> "Follow-up for:"
  | HomeTitle -> "University Registration Center for Study Participants"
  | I18nTitle -> "Translations"
  | NoEntries field ->
    Format.asprintf "There are no %s yet." (Locales_en.field_to_string field)
  | OurPartners -> "Our partners"
  | ProfileCompletionTitle -> "Profile completion"
  | LocationFileNew -> "Add file to location"
  | LocationListTitle -> "Location"
  | LocationNewTitle -> "Create new location"
  | LocationNoFiles -> "There are no files for this location."
  | LocationNoSessions -> "No sessions found for this location."
  | LoginTitle -> "Login"
  | MailingDetailTitle start ->
    Format.asprintf "Mailing at %s" (Utils_time.formatted_date_time start)
  | MailingNewTitle -> "Create new mailing"
  | RateTotalSent number ->
    Format.asprintf "Totally generated invitations: %d" number
  | ResetPasswordLink | ResetPasswordTitle -> "Reset password"
  | Reminder -> "Reminder"
  | SentInvitations -> "Sent invitations"
  | SessionDetailTitle start ->
    Format.asprintf "Session at %s" (Utils_time.formatted_date_time start)
  | SessionReminderDefaultLeadTime leadtime ->
    Format.asprintf
      "The experiment default lead time is: %s"
      (leadtime |> Pool_common_utils.Time.formatted_timespan)
  | SessionReminderDefaultText text ->
    Format.asprintf "The experiment default reminder text is:\n\n %s" text
  | SessionReminderDefaultSubject text ->
    Format.asprintf "The experiment default subject is:\n\n %s" text
  | SessionReminder -> "Session reminder"
  | SessionIndent -> "Indentations group follow-up sessions."
  | SessionRegistrationTitle -> "Register for this session"
  | SignUpAcceptTermsAndConditions -> "I accept the terms and conditions."
  | SignUpCTA -> "Register now to participate in economic experiments"
  | SignUpTitle -> "Sign up"
  | SortUngroupedFields -> "Sort ungrouped fields"
  | SwitchChronological -> "Switch to chronological view"
  | SwitchGrouped -> "Switch to grouped view"
  | TermsAndConditionsTitle -> "Terms and Conditions"
  | TextTemplates -> "text templates"
  | UserProfileDetailsSubtitle -> "Personal details"
  | UserProfileLoginSubtitle -> "Login information"
  | UserProfilePausedNote ->
    "You paused all notifications for your user! (Click 'edit' to update this \
     setting)"
  | Validation -> "Validation"
  | WaitingListIsDisabled -> "The waiting list is disabled."
;;

let nav_link_to_string = function
  | Admins -> "Admins"
  | Assignments -> "Assignments"
  | Contacts -> "Contacts"
  | CustomFields -> "Fields"
  | Dashboard -> "Dashboard"
  | Experiments -> "Experiments"
  | Filter -> "Filter"
  | I18n -> "Translations"
  | Invitations -> "Invitations"
  | LoginInformation -> "Login information"
  | Login -> "Login"
  | Locations -> "Locations"
  | Logout -> "Logout"
  | Mailings -> "Mailings"
  | Overview -> "Overview"
  | PersonalDetails -> "Personal details"
  | Profile -> "Profile"
  | Sessions -> "Sessions"
  | Settings -> "Settings"
  | Tenants -> "Tenants"
  | WaitingList -> "Waiting list"
;;

let rec hint_to_string = function
  | AllowUninvitedSignup ->
    "Contacts who have not been invited will be able to sign up for the \
     experiment."
  | AssignContactFromWaitingList ->
    "Select the session to which you want to assign the contact."
  | CustomFieldAdminInputOnly ->
    Format.asprintf
      "This option excludes \"%s\"."
      (Locales_en.field_to_string Entity_message.Field.Required
      |> CCString.capitalize_ascii)
  | CustomFieldAdminViewOnly ->
    Format.asprintf
      "This option implies \"%s\"."
      (Locales_en.field_to_string Entity_message.Field.AdminInputOnly
      |> CCString.capitalize_ascii)
  | CustomFieldContactModel ->
    "Questions that contacts can, or must, answer. Based on this information, \
     contacts are invited to take part in experiments."
  | CustomFieldExperimentModel -> "Customziable attributes for experiments."
  | CustomFieldSessionModel -> "Customziable attributes for sessions."
  | CustomFieldGroups ->
    Format.asprintf
      {|Groups to group custom fields by. Grouping custom fields does not have any effect on their functionality. It only has a graphical impact.

     %s
     |}
      (hint_to_string (CustomFieldSort Entity_message.Field.CustomFieldGroups))
  | CustomFieldSort field ->
    Format.asprintf
      "The %s will be displayed to the contacts in this order."
      (Locales_en.field_to_string field)
  | CustomHtmx s -> s
  | DirectRegistrationDisbled ->
    "If this option is enabled, contacts can join the waiting list but cannot \
     directly enroll in the experiment."
  | Distribution ->
    "The distribution can be used to influence which invitations are sent \
     first."
  | ExperimentAssignment ->
    "All assignments of contacts to sessions of this experiment, sorted by \
     session."
  | ExperimentMailings ->
    {|Invitation mailings of this experiment. 'Rate' defines the maximum generated invitations per hour.

    Started mailings can no longer be deleted.|}
  | ExperimentWaitingList ->
    "Contacts that have been invited to this experiment and have placed \
     themselves on the waiting list. They have to be manually assigned to a \
     session."
  | ExperimentSessions ->
    {|All existing session of this experiment.
      Once someone has registered for the session, it can no longer be deleted.
    |}
  | LocationFiles ->
    "Additional information about the location, such as directions. Contacts \
     who are participating in a session at this location can access access \
     these files."
  | LocationSessions ->
    "Future sessions, that will be conducted at this location."
  | Locations ->
    "Locations, where experiments are conducted. Every session has to have a \
     location."
  | I18nText str -> str
  | NumberIsSecondsHint -> "Nr. of seconds"
  | NumberIsDaysHint -> "Nr. of days"
  | NumberIsWeeksHint -> "Nr. of weeks"
  | Overbook ->
    "Number of subjects that can enroll in a session in addition to the \
     maximum number of contacts."
  | Rate -> "Max. generated Invitations per hour"
  | RateDependencyWith ->
    "There are other mailings running at the same time, see its details \
     bellow. In case the sum of all rates reaches the maximum of the server, \
     they will automatically get reduced."
  | RateDependencyWithout ->
    "There are currently no other mailings running in the specified time range."
  | RateNumberPerMinutes (per_n_minutes, number) ->
    Format.asprintf
      "Generates max %i new invitations every %d minutes"
      (number +. 0.5 |> CCFloat.round |> CCInt.of_float)
      per_n_minutes
  | RegistrationDisabled ->
    "If this option is activated, contacts can neither register nor join the \
     waiting list. The experiment is not visible to the contacts."
  | SelectedDateIsPast -> "The selected date is in the past."
  | SessionReminderLanguageHint ->
    "If you provide a custom reminder text, select its language here."
  | SessionRegistrationHint -> "The registration for a session is binding."
  | SignUpForWaitingList ->
    "The recruitment team will contact you, to assign you to a session, if \
     there is a free place."
  | TemplateTextElementsHint ->
    "The following text elements can be used inside the templates:"
  | TimeSpanPickerHint -> "Hours and minutes"
;;

let confirmable_to_string confirmable =
  (match confirmable with
   | CancelSession -> "session", "cancel", None
   | DeleteCustomField -> "field", "delete", None
   | DeleteCustomFieldOption -> "option", "delete", None
   | DeleteEmailSuffix -> "email suffix", "delete", None
   | DeleteExperiment -> "experiment", "delete", None
   | DeleteFile -> "the file", "delete", None
   | DeleteMailing -> "mailing", "delete", None
   | DeleteSession -> "session", "delete", None
   | PublisCustomField ->
     ( "field an all associated options"
     , "publish"
     , Some "You will not be able to delete it field anymore." )
   | PublisCustomFieldOption ->
     "option", "publish", Some "You will not be able to delete the it anymore."
   | StopMailing -> "mailing", "stop", None)
  |> fun (obj, action, additive) ->
  Format.asprintf "Are you sure you want to %s the %s?" action obj
  |> fun msg ->
  additive
  |> CCOption.map_or ~default:msg (fun additive ->
       Format.asprintf "%s %s" msg additive)
;;
