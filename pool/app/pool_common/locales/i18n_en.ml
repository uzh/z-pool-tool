open Entity_i18n

let to_string = function
  | Canceled -> "Canceled"
  | ContactWaitingListEmpty -> "You are currently not on any waiting list."
  | ContactWaitingListTitle -> "On the waiting list"
  | DashboardProfileCompletionText ->
    "Your profile is incomplete. To be invited to more experiments, fulfill \
     your profile."
  | DashboardProfileCompletionTitle -> "Profile completion"
  | DashboardTitle -> "Dashboard"
  | DontHaveAnAccount -> "Don't have an account?"
  | EmailConfirmationNote ->
    "Please check your emails and confirm your address first."
  | EmailConfirmationTitle -> "Email confirmation"
  | EmtpyList field ->
    Format.asprintf
      "Currently, there are no %s available."
      (Locales_en.field_to_string field)
  | ExperimentContactEnrolledNote ->
    "You signed up for the following session(s):"
  | ExperimentNewTitle -> "Create new experiment"
  | ExperimentListTitle -> "Experiments"
  | ExperimentListEmpty ->
    "Currently there are no experiments you can participate."
  | ExperimentListPublicTitle -> "Register for experiment sessions"
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
  | NotifyVia -> "Notify via"
  | OurPartners -> "Our partners"
  | ProfileCompletionText ->
    {|The following information is required to be invited to experiments. Further information can be entered in your profile afterwards.

    You will be considered for more experiments, the more complete your profile is.|}
  | LocationFileNew -> "Add file to location"
  | LocationListTitle -> "Location"
  | LocationNewTitle -> "Create new location"
  | LocationNoFiles -> "There are no files for this location."
  | LocationNoSessions -> "No sessions found for this location."
  | LoginTitle -> "Login"
  | MailingDetailTitle start ->
    Format.asprintf "Mailing at %s" (Utils_time.formatted_date_time start)
  | MailingExperimentSessionFullyBooked ->
    "All sessions are fully booked. No invitations will be sent (independent \
     if mailings are active at the moment).\n\n\
     Add additional sessions to the experiment."
  | MailingNewTitle -> "Create new mailing"
  | RateTotalSent number ->
    Format.asprintf "Totally generated invitations: %d" number
  | ResetPasswordLink | ResetPasswordTitle -> "Reset password"
  | Reminder -> "Reminder"
  | RoleApplicableToAssign -> "Applicable users"
  | RoleCurrentlyAssigned -> "Currently assigned"
  | RoleCurrentlyNoneAssigned field ->
    Format.asprintf
      "Currently there are no %s assigned."
      (Locales_en.field_to_string field)
  | SentInvitations -> "Sent invitations"
  | SessionDetailTitle start ->
    Format.asprintf "Session at %s" (Utils_time.formatted_date_time start)
  | SessionIndent -> "Indentations group follow-up sessions."
  | SessionReminderDefaultLeadTime leadtime ->
    Format.asprintf
      "The default lead time is: %s"
      (leadtime |> Pool_common_utils.Time.formatted_timespan)
  | SessionReminder -> "Session reminder"
  | SessionRegistrationTitle -> "Register for this session"
  | SignUpAcceptTermsAndConditions -> "I accept the terms and conditions."
  | SignUpCTA -> "Register now to participate in economic experiments."
  | SignUpTitle -> "Sign up"
  | SortUngroupedFields -> "Sort ungrouped fields"
  | SwitchChronological -> "Switch to chronological view"
  | SwitchGrouped -> "Switch to grouped view"
  | TermsAndConditionsTitle -> "Terms and Conditions"
  | TextTemplates -> "text templates"
  | UpcomingSessionsListEmpty ->
    "You are not currently enrolled in any upcoming sessions."
  | UpcomingSessionsTitle -> "Your upcoming sessions"
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
  | Field field -> Locales_en.field_to_string field |> CCString.capitalize_ascii
  | Filter -> "Filter"
  | I18n -> "Translations"
  | Invitations -> "Invitations"
  | Locations -> "Locations"
  | Login -> "Login"
  | LoginInformation -> "Login information"
  | Logout -> "Logout"
  | Mailings -> "Mailings"
  | MessageTemplates -> "Message templates"
  | Overview -> "Overview"
  | PersonalDetails -> "Personal details"
  | Profile -> "Profile"
  | Queue -> "Queued jobs"
  | Schedules -> "Schedules"
  | Sessions -> "Sessions"
  | Settings -> "Settings"
  | Smtp -> "Email Server"
  | SystemSettings -> "System settings"
  | Tenants -> "Tenants"
  | Users -> "Users"
  | WaitingList -> "Waiting list"
;;

let rec hint_to_string = function
  | AllowUninvitedSignup ->
    "Contacts who have not been invited will be able to sign up for the \
     experiment."
  | AssignContactFromWaitingList ->
    "Select the session to which you want to assign the contact."
  | ContactOnWaitingList ->
    "You are on the waiting list. The recruitment team will assign you to a \
     session."
  | ContactProfileVisibleOverride ->
    "If you overwrite these values, the changes will be visible to the contact."
  | CustomFieldAdminInputOnly ->
    Format.asprintf
      "This option excludes \"%s\"."
      (Locales_en.field_to_string Entity_message.Field.Required
      |> CCString.capitalize_ascii)
  | CustomFieldAdminOverride ->
    "Allows administrators to override the answers specified by the contact. \
     Contacts cannot view the overridden answers."
  | CustomFieldAdminOverrideUpdate ->
    "Unchecking this option will make the filter ignore all currently existing \
     overridden answers."
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
    {|Groups to group custom fields by. Grouping custom fields does not have any effect on their functionality. It only has a graphical impact.|}
  | CustomFieldNoContactValue -> "Not answered by contact"
  | CustomFieldOptionsCompleteness ->
    "Make sure this list is complete or add an option to select if none of the \
     others are applicable."
  | CustomFieldSort field ->
    Format.asprintf
      "The %s will be displayed to the contacts in this order."
      (Locales_en.field_to_string field)
  | CustomFieldTypeText ->
    "Please take into account that the data quality is lower for text entries. \
     If the data can be collected in another form, this is preferable."
  | CustomFieldTypeSelect ->
    "You will be able to create the available options in the section 'Option' \
     after the custom field is created."
  | CustomFieldTypeMultiSelect -> hint_to_string CustomFieldTypeSelect
  | CustomHtmx s -> s
  | DirectRegistrationDisbled ->
    "If this option is enabled, contacts can join the waiting list but cannot \
     directly enroll in the experiment."
  | Distribution ->
    "The distribution can be used to influence which invitations are sent \
     first."
  | EmailPlainText -> "Make sure to show links as plain text."
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
  | ExperimentSessionsPublic ->
    "Please note: Maybe sessions or complete experiments are no longer \
     displayed, although listed in the email. Once all the available seats are \
     assigned a session, it is no longer displayed."
  | FilterContacts ->
    "Define the criteria by which contacts will be invited to this experiment."
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
  | ScheduleEvery sec ->
    sec
    |> Pool_common_utils.Time.formatted_timespan
    |> Format.asprintf "every %s"
  | ScheduleAt time ->
    time
    |> Pool_common_utils.Time.formatted_date_time
    |> Format.asprintf "at %s"
  | ScheduledIntro ->
    {|Information about all periodic background processes.

      Note: When the application restarts all active schedules get stopped.
      |}
  | SessionCancelMessage ->
    "This reason will be provided to all contacts assigned to this session."
  | SessionClose ->
    {|S: the contact showed up
    P: the contact participated in the experiment

    To set 'participated', 'show up' is required.
    |}
  | SelectedDateIsPast -> "The selected date is in the past."
  | SessionReminderLanguageHint ->
    "If you provide a custom reminder text, select its language here."
  | SessionRegistrationHint -> "The registration for a session is binding."
  | SessionRegistrationFollowUpHint ->
    "The registration for a session incl. all follow up sessions is binding."
  | SignUpForWaitingList ->
    "The recruitment team will contact you, to assign you to a session, if \
     there is a free place."
  | SmtpSettingsIntro ->
    {|The following configuration is used by the email service.

    Note: When using the mechanism "LOGIN" a username and password are required.
    |}
  | TemplateTextElementsHint ->
    "The following text elements can be used inside the templates:"
  | TimeSpanPickerHint ->
    "Time duration in hours. '1.5' corresponds to 1h 30m. '0.75' corresponds \
     to 45min."
;;

let confirmable_to_string confirmable =
  (match confirmable with
   | CancelAssignment -> "assignment", "cancel", None
   | CancelSession -> "session", "cancel", None
   | DeleteCustomField -> "field", "delete", None
   | DeleteCustomFieldOption -> "option", "delete", None
   | DeleteEmailSuffix -> "email suffix", "delete", None
   | DeleteExperiment -> "experiment", "delete", None
   | DeleteExperimentFilter -> "filter", "delete", None
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
