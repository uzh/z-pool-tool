open Entity_i18n

let capitalize = CCString.capitalize_ascii
let error_to_string = Locales_en.error_to_string

let to_string = function
  | Activity -> "activity"
  | Address -> "address"
  | AdminComment -> "admin comment"
  | AssignmentEditTagsWarning ->
    "Please note that editing the assignment does not assign or remove any \
     tags from the contact that may have been assigned by participating in \
     this session. If this is required, please get in touch with a person with \
     the necessary permissions."
  | AssignmentListEmpty -> "There are no assignments for this session."
  | AvailableSpots -> "Available spots"
  | Canceled -> "Canceled"
  | Closed -> "Closed"
  | ContactWaitingListEmpty -> "You are currently not on any waiting list."
  | ContactWaitingListTitle -> "On the waiting list"
  | CustomFieldsSettings ->
    "In the following list, you can determine in which table the custom data \
     should be displayed in addition to the contact details."
  | DashboardProfileCompletionText ->
    "Your profile is incomplete. To be invited to more experiments, fulfill \
     your profile."
  | DashboardProfileCompletionTitle -> "Profile completion"
  | DashboardTitle -> "Dashboard"
  | DeletedAssignments -> "Deleted assignments"
  | Disabled ->
    Locales_en.field_to_string Entity_message_field.Disabled
    |> CCString.capitalize_ascii
  | DontHaveAnAccount -> "Don't have an account?"
  | EmailConfirmationNote ->
    "Please check your emails and confirm your address first."
  | EmailConfirmationTitle -> "Email confirmation"
  | EmptyListGeneric -> "No entries were found."
  | EmtpyList field ->
    Format.asprintf
      "Currently, there are no %s available."
      (Locales_en.field_to_string field)
  | EnrollInExperiment -> "Enroll in experiment"
  | ExperimentListEmpty ->
    "Currently there are no experiments you can participate."
  | ExperimentListPublicTitle -> "Register for experiment sessions"
  | ExperimentListTitle -> "Experiments"
  | ExperimentMessagingSubtitle -> "Messaging"
  | ExperimentNewTitle -> "Create new experiment"
  | ExperimentSessionReminderHint ->
    "These are default settings for the sessions of this experiment. These \
     settings can be overwritten for each session."
  | ExperimentWaitingListTitle -> "Waiting list"
  | Files -> "Files"
  | FilterContactsDescription ->
    {|<p>To start inviting contacts to this experiment, follow those steps:</p>
    <ol>
      <li>Create a filter using one or multiple conditions to define which contacts you would like to include in this experiment.</li>
      <li>Create the sessions on which you want to perform the experiment.</li>
      <li>Create one or more mailings to start sending out emails to these participants.</li>
    </ol>|}
  | FilterNrOfContacts ->
    "Number of contacts meeting the criteria of this filter:"
  | FilterNrOfSentInvitations -> "Number of contacts already invited:"
  | FollowUpSessionFor -> "Follow-up for:"
  | ImportConfirmationNote ->
    "Please enter a new password. The rest of your data has been automatically \
     taken over."
  | ImportConfirmationTitle -> "New password"
  | ImportPendingNote ->
    "The import of your user is not completed yet. Please check your inbox or \
     contact an administrator."
  | ImportPendingTitle -> "Pending import"
  | InvitationsStatistics -> "Invitation statistics"
  | InvitationsStatisticsIntro ->
    "This table shows how often contacts received the invitation to this \
     experiment."
  | JobCloneOf -> "This job is a clone of"
  | LocationDetails -> "Location details"
  | LocationFileNew -> "Add file to location"
  | LocationListTitle -> "Location"
  | LocationNewTitle -> "Create new location"
  | LocationNoFiles -> "There are no files for this location."
  | LocationNoSessions -> "No sessions found for this location."
  | LoginTitle -> "Login"
  | MailingDetailTitle start ->
    Format.asprintf "Mailing at %s" (Utils_time.formatted_date_time start)
  | MailingDistributionDescription ->
    {|<ol>
    <li>Select by which field and in which order you want to sort the contacts.</li>
    <li>Press the 'add' button to add the sorting parameter.</li>
    <li>Repeat that to add more parameters. You can sort them by dragging and dropping them.</li>
  </ol>|}
  | MailingExperimentSessionFullyBooked ->
    "All sessions are fully booked. No invitations will be sent (independent \
     if mailings are active at the moment).\n\n\
     Add additional sessions to the experiment."
  | MailingNewTitle -> "Create new mailing"
  | MessageHistory name -> Format.asprintf "Message history of %s" name
  | NoEntries field ->
    Format.asprintf "There are no %s yet." (Locales_en.field_to_string field)
  | Note -> "Note"
  | OurPartners -> "Our partners"
  | Past -> "Past"
  | PastExperimentListPublicTitle -> "Experiments participated"
  | PastSessionsTitle -> "Your past sessions"
  | PoolStatistics -> "Pool statistics"
  | ProfileCompletionText ->
    {|The following information is required to be invited to experiments. Further information can be entered in your profile afterwards.

    You will be considered for more experiments, the more complete your profile is.|}
  | Reminder -> "Reminder"
  | ResendReminders -> "Resend reminders"
  | ResetPasswordLink | ResetPasswordTitle -> "Reset password"
  | RoleApplicableToAssign -> "Applicable users"
  | RoleCurrentlyAssigned -> "Currently assigned"
  | RoleCurrentlyNoneAssigned field ->
    Format.asprintf
      "Currently there are no %s assigned."
      (Locales_en.field_to_string field)
  | RolesGranted -> "Granted roles"
  | SelectedTags -> "Currently assigned tags"
  | SelectedTagsEmpty -> "No tags assigned"
  | SentInvitations -> "Sent invitations"
  | SessionCloseScreen -> "Session close screen"
  | SessionDetailTitle start ->
    Format.asprintf "Session at %s" (Utils_time.formatted_date_time start)
  | SessionIndent -> "Indentations group follow-up sessions."
  | SessionRegistrationTitle -> "Register for this session"
  | SessionReminder -> "Session reminder"
  | SignUpAcceptTermsAndConditions -> "I accept the terms and conditions."
  | SignUpTitle -> "Sign up"
  | SortUngroupedFields -> "Sort ungrouped fields"
  | SwapSessionsListEmpty ->
    "No sessions were found to which you can assign this contact."
  | SwitchChronological -> "Switch to chronological view"
  | SwitchGrouped -> "Switch to grouped view"
  | TermsAndConditionsLastUpdated ptime ->
    Format.asprintf
      "Last updated: %s"
      (Pool_common_utils.Time.formatted_date ptime)
  | TermsAndConditionsTitle -> "Terms and Conditions"
  | TermsAndConditionsUpdated ->
    "We have recently changed our terms and conditions. Please read and accept \
     them to continue."
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
  | ContactInformation -> "Contact information"
  | Contacts -> "Contacts"
  | Credits -> "Credits"
  | CustomFields -> "Fields"
  | Dashboard -> "Dashboard"
  | Experiments -> "Experiments"
  | ExternalDataIds -> "External data ids"
  | Field field -> Locales_en.field_to_string field |> CCString.capitalize_ascii
  | Filter -> "Filter"
  | I18n -> "Texts"
  | Invitations -> "Invitations"
  | Locations -> "Locations"
  | Login -> "Login"
  | LoginInformation -> "Login information"
  | Logout -> "Logout"
  | Mailings -> "Mailings"
  | MessageHistory -> "Message history"
  | MessageTemplates -> "Message templates"
  | OrganisationalUnits -> "Organisational units"
  | Overview -> "Overview"
  | ParticipationTags -> "Participation tags"
  | PersonalDetails -> "Personal details"
  | PrivacyPolicy -> "Privacy policy"
  | Profile -> "Profile"
  | Queue -> "Queued jobs"
  | RolePermissions -> "Role permission"
  | Schedules -> "Schedules"
  | Sessions -> "Sessions"
  | Settings -> "Settings"
  | Smtp -> "Email Server (SMTP)"
  | SystemSettings -> "System settings"
  | Tags -> "Tags"
  | Tenants -> "Tenants"
  | TextMessages -> "Text messages"
  | Users -> "Users"
  | WaitingList -> "Waiting list"
;;

let rec hint_to_string = function
  | AdminOverwriteContactValues ->
    {|If you overwrite one of the following values, this is not apparent to the contact. If a value entered by the contact is overridden, the overridden value is displayed below the input field.

When inviting contacts, the filter will prefer the overriding value if both are available.|}
  | AllowUninvitedSignup ->
    "All contacts (invited or not) will be able to sign up for the experiment."
  | AssignContactFromWaitingList ->
    "Select the session to which you want to assign the contact."
  | AssignmentConfirmationMessageFollowUps ->
    "You also have been assigned to the following followup sessions:"
  | AssignmentsMarkedAsClosed ->
    "These assignments have been marked as deleted. Provided that the contacts \
     still meet the experiment criteria, they can register for sessions again."
  | ContactCurrentCellPhone cell_phone ->
    Format.asprintf "Your current phone number is %s." cell_phone
  | ContactEnrollmentDoesNotMatchFilter ->
    "The contact does not meet the criteria specified in the filter for this \
     experiment, but can still be enrolled."
  | ContactEnrollmentRegistrationDisabled ->
    "Registration for this experiment is currently disabled. Contacts cannot \
     enroll themselves for this experiment."
  | ContactEnterCellPhoneToken cell_phone ->
    Format.asprintf
      "Please enter the verification code we sent yout to %s. The code is \
       valid for one hour."
      cell_phone
  | ContactLanguage ->
    "Some experiments choose to communicate in a different language, \
     disregarding your contact language."
  | ContactNoCellPhone -> "You have not yet verified a phone number."
  | ContactOnWaitingList ->
    "You are on the waiting list. The recruitment team will assign you to a \
     session."
  | ContactPhoneNumberVerificationWasReset ->
    "You can enter a different phone number now."
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
  | CustomFieldAnsweredOnRegistration ->
    "This field has already been answered by the contact during registration \
     and can no longer be changed by the contact him- or herself."
  | CustomFieldContactModel ->
    "Questions that contacts can, or must, answer. Based on this information, \
     contacts are invited to take part in experiments."
  | CustomFieldExperimentModel -> "Customziable attributes for experiments."
  | CustomFieldGroups ->
    {|Groups to group custom fields by. Grouping custom fields does not have any effect on their functionality. It only has a graphical impact.|}
  | CustomFieldNoContactValue -> "Not answered by contact"
  | CustomFieldOptionsCompleteness ->
    "Make sure this list is complete or add an option to select if none of the \
     others are applicable."
  | CustomFieldPromptOnRegistration ->
    "If this option is enabled, this field is already prompted during \
     registration, but is no longer displayed to the contact in the user \
     profile."
  | CustomFieldSessionModel -> "Customziable attributes for sessions."
  | CustomFieldSort field ->
    Format.asprintf
      "The %s will be displayed to the contacts in this order."
      (Locales_en.field_to_string field)
  | CustomFieldTypeMultiSelect -> hint_to_string CustomFieldTypeSelect
  | CustomFieldTypeSelect ->
    "You will be able to create the available options in the section 'Option' \
     after the custom field is created."
  | CustomFieldTypeText ->
    "Please take into account that the data quality is lower for text entries. \
     If the data can be collected in another form, this is preferable."
  | CustomHtmx s -> s
  | DefaultReminderLeadTime lead_time ->
    Format.asprintf
      "If left blank, the default lead time of %s is applied."
      (lead_time |> Utils_time.formatted_timespan)
  | DirectRegistrationDisbled ->
    "If this option is enabled, contacts can join the waiting list but cannot \
     directly enroll in the experiment."
  | Distribution ->
    "The distribution can be used to influence which invitations are sent \
     first."
  | EmailPlainText ->
    {|Using plain text email as a fallback ensures universal readability and accessibility. You can copy the rich text from above by using the button on the top right corner of this textarea.
Make sure to show links and URLs as plain text.
  |}
  | ExperimentAssignment ->
    "All assignments of contacts to sessions of this experiment, sorted by \
     session."
  | ExperimentContactPerson ->
    "The selected user's email address will be used as 'reply-to' address for \
     all experiment-related emails."
  | ExperimentLanguage ->
    "If an experiment language is defined, all messages regarding this \
     experiment will be sent in this language, disregarding the contact \
     language."
  | ExperimentMailings ->
    {|Invitation mailings of this experiment. The limit defines the number of invitations sent by the mailing withing it's duration.

    Check the No. Invitations to see how many of the invitations where already sent/handled.
    In case there are multiple mailings running at the same time, the server might has to reduce the amount and thus doesn't reach the desired limit.

    Started mailings can no longer be deleted.|}
  | ExperimentMailingsRegistrationDisabled ->
    {|Registration to this experiment is currently disabled. Invitations will still be sent out if a mailing is created, but contacts won't be able to sign up for a session.|}
  | ExperimentMessageTemplates ->
    {|Messages sent to contacts regarding this experiment or session can be customized if you want to add or remove information. The template is selected in the following hierarchy: session-specific > experiment-specific > default.

If an experiment language is specified, all messages will be sent in this language. The messages will be sent in the contact display language if no experiment-specific language is defined.

By clicking on the template labels below you can open the default text message:
|}
  | ExperimentSessions ->
    {|All existing session of this experiment.
      Once someone has registered for the session, it can no longer be deleted.
    |}
  | ExperimentSessionsPublic ->
    "Please note: Maybe sessions or completed experiments are no longer \
     displayed, although listed in the email. Once all the available seats are \
     assigned a session, it is no longer displayed."
  | ExperimentWaitingList ->
    "Contacts that have been invited to this experiment and have placed \
     themselves on the waiting list. They have to be manually assigned to a \
     session."
  | ExternalDataRequired ->
    "An external data identifier is required for every assignement (latest \
     when a session is closed)."
  | GtxKeyMissing ->
    "No GTX Api key is stored, which is why no text messages are sent."
  | GtxKeyStored -> "A GTX Api key is stored. Text message service is running."
  | I18nText str -> str
  | LocationFiles ->
    "Additional information about the location, such as directions. Contacts \
     who are participating in a session at this location can access access \
     these files."
  | LocationsIndex ->
    "Locations, where experiments are conducted. Every session has to have a \
     location."
  | MailingLimit -> "Max. generated Invitations during the mailing."
  | MessageTemplateAccountSuspensionNotification ->
    "This message will be sent to a user after the account has been \
     temporarily suspended because of too many failed login attempts."
  | MessageTemplateAssignmentConfirmation ->
    "This message will be sent to contacts after successfully registering for \
     a session."
  | MessageTemplateAssignmentSessionChange ->
    "This message will be sent to contacts after they have been assigned to \
     another session by an administrator."
  | MessageTemplateContactEmailChangeAttempt ->
    "This message will be sent to a user after someone tries to change their \
     email address to an existing one."
  | MessageTemplateContactRegistrationAttempt ->
    "This message will be sent to a user after a registration attempt using an \
     existing email address."
  | MessageTemplateEmailVerification ->
    "This email is used to verify new email addresses after changing an \
     account email address. You can ignore the SMS text input."
  | MessageTemplateExperimentInvitation ->
    "This message is sent to invite contacts to experiments."
  | MessageTemplatePasswordChange ->
    "This message is sent to notify users that the account password has been \
     changed."
  | MessageTemplatePasswordReset ->
    "This message sends the password reset token to the given address."
  | MessageTemplatePhoneVerification ->
    "This message sends the phone number verification token to the contact's \
     phone. You can ignore the email and plain text."
  | MessageTemplateProfileUpdateTrigger ->
    "This message is used to notify contacts who last updated their profile a \
     while ago and request them to control their personal information."
  | MessageTemplateSessionCancellation ->
    "This message is used to notify contacts about the cancellation of a \
     session."
  | MessageTemplateSessionReminder ->
    "This message reminds contacts about upcoming sessions they are signed up \
     for."
  | MessageTemplateSessionReschedule ->
    "This message is used to notify contacts about the rescheduling of a \
     session."
  | MessageTemplateSignupVerification ->
    "This email is used to verify new email addresses after signing up. You \
     can ignore the SMS text input."
  | MessageTemplateUserImport ->
    "This message informs imported contacts about the migration to the \
     Z-Pool-Tool and contains the token they need to reset their password."
  | MessageTemplateWaitingListConfirmation ->
    "This message confirms the successful enrollment to an experiment waiting \
     list."
  | MissingMessageTemplates ->
    "The following message templates are missing. The default message will be \
     sent to contacts who selected one of those languages as their \
     communication language."
  | NumberIsDaysHint -> "Nr. of days"
  | NumberIsSecondsHint -> "Nr. of seconds"
  | NumberIsWeeksHint -> "Nr. of weeks"
  | NumberMax i -> error_to_string (Entity_message.NumberMax i)
  | NumberMin i -> error_to_string (Entity_message.NumberMin i)
  | Overbook ->
    "Number of subjects that can enroll in a session in addition to the \
     maximum number of contacts."
  | PartialUpdate ->
    "The following form will save the changed values immediately. You do not \
     need to submit the form."
  | ParticipationTagsHint ->
    "Tags, which are automatically assigned to participants after they have \
     participated in a session of this experiment."
  | PauseAccountAdmin ->
    "As long the account is paused, the contact will not be invited to any \
     further experiments."
  | PauseAccountContact ->
    "As long as your account is paused, you will not be invited to any further \
     experiments."
  | PromoteContact ->
    "Attention: one-time action. The contact is promoted to an admin, who is \
     subsequently no longer invited for experiments and can no longer register \
     for such."
  | RateDependencyWith ->
    "There are other mailings running at the same time. See its details below. \
     If the sum of all reaches the server's maximum, they will automatically \
     get reduced."
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
  | RescheduleSession ->
    "When you reschedule a session, all registered contacts are automatically \
     informed."
  | ResendRemindersChannel ->
    "If you choose to resend the reminders as text messages, contacts without \
     a verified cell phone number will receive the reminder via email."
  | ResendRemindersWarning ->
    {|No automatic reminders have been sent for this session yet. Make sure that the message template is correct if you want to trigger the reminders now.

If you trigger the reminders manually now, no more automatic reminders will be sent via the selected message channel.|}
  | ResetInvitations ->
    "Resets invitations, all previous invitations up to the now will be \
     ignored."
  | ResetInvitationsLastReset reset_at ->
    Format.asprintf
      "The invitations were last reset on <strong>%s</strong>."
      (Utils_time.formatted_date_time reset_at)
  | RoleIntro (singular, plural) ->
    Format.asprintf
      "If no %s is specified, the role includes all %s."
      (Locales_en.field_to_string singular)
      (Locales_en.field_to_string plural)
  | RolePermissionsIntro ->
    {|All existing permissions which are defined for roles of the tenant.|}
  | ScheduleAt time ->
    time
    |> Pool_common_utils.Time.formatted_date_time
    |> Format.asprintf "at %s"
  | ScheduledIntro ->
    {|Information about all periodic background processes.

      Note: When the application restarts all active schedules get stopped.
      |}
  | ScheduleEvery sec ->
    sec
    |> Pool_common_utils.Time.formatted_timespan
    |> Format.asprintf "every %s"
  | SearchByFields fields ->
    Format.asprintf
      "Search by: %s"
      (fields |> CCList.map Locales_en.field_to_string |> CCString.concat ", ")
  | SelectedDateIsPast -> "The selected date is in the past."
  | SelectedOptionsCountMax i ->
    error_to_string (Entity_message.SelectedOptionsCountMax i)
  | SelectedOptionsCountMin i ->
    error_to_string (Entity_message.SelectedOptionsCountMin i)
  | SessionCancellationMessageFollowUps ->
    "Associated follow-up sessions were canceled as well:"
  | SessionCancellationWithFollowups ->
    {|Cancelling this session will also cancel all follow-up sessions.

  The following follow-up sessions exist:|}
  | SessionCancelMessage ->
    "This reason will be provided to all contacts assigned to this session."
  | SessionCloseHints ->
    Format.asprintf
      {|<strong>%s</strong> and <strong>%s</strong> are mutually exclusive.<br>
    If none of the two checkboxes is checked, it equals to 'show up but did not participate'|}
      (Locales_en.field_to_string Entity_message_field.NoShow |> capitalize)
      (Locales_en.field_to_string Entity_message_field.Participated
       |> capitalize)
  | SessionCloseLegendNoShow -> "the contact did not show up"
  | SessionCloseLegendParticipated ->
    "the contact participated in the experiment"
  | SessionCloseNoParticipationTagsSelected ->
    "No tags were selected to be assigned to the participants who participated \
     in this experiment."
  | SessionCloseParticipationTagsSelected ->
    "The following tags are assigned to all participants who took part in this \
     experiment:"
  | SessionRegistrationFollowUpHint ->
    "The registration for a session incl. all follow up sessions is binding."
  | SessionRegistrationHint -> "The registration for a session is binding."
  | SessionReminderLanguageHint ->
    "If you provide a custom reminder text, select its language here."
  | SessionReminderLeadTime ->
    "The lead time determines how long before the start of the session the \
     reminders are sent to the contacts"
  | SettingsNoEmailSuffixes ->
    "There are no email suffixes defined that are allowed. This means that all \
     email suffixes are allowed."
  | SignUpForWaitingList ->
    "The recruitment team will contact you, to assign you to a session, if \
     there is a free place."
  | SmtpSettingsDefaultFlag ->
    "Attention: If another SMTP configuration is marked as default, it will be \
     overwritten. Only one configuration can be marked as default."
  | SmtpSettingsIntro ->
    {|The following configuration is used by the email service.

    Note: When using the mechanism "LOGIN" a username and password are required.
    |}
  | SwapSessions ->
    {|Changing the session will only change the session of this assignment. If follow-up assignments exists, they must be updated manually.

Only sessions with open spots can be selected.|}
  | TagsIntro ->
    "The defined tags can be added to several types (e.g. contacts). The tags \
     can be used by the experiment filter to eighter include or exclude them."
  | TemplateTextElementsHint ->
    "The following text elements can be used inside the templates. Click on \
     the labels to copy them to the clipboard."
  | TenantDatabaseLabel ->
    "A label that is an identifier for the tenant, e.g. 'econ-uzh'. The label \
     must be unique."
  | TenantDatabaseUrl ->
    {|The database URL, according to the following scheme:
     mariadb://<user>:<pw>@<host>:<port>/<database>|}
  | TenantUrl -> "The URL of the tenant without protocol, e.g.: pool.uzh.ch"
  | TestPhoneNumber ->
    "Please provide a phone number where we can send a single test message to \
     verify the api key. The number must have the format +41791234567."
  | TextLengthMax i -> error_to_string (Entity_message.TextLengthMax i)
  | TextLengthMin i -> error_to_string (Entity_message.TextLengthMin i)
  | UserImportInterval ->
    {|<p>Define after how many days a reminder will be sent to contacts that have not confirmed the import yet.</p>
<p><strong>The 'second reminder' setting defines how long after the first reminder the second reminder is sent.</strong></p>|}
  | WaitingListPhoneMissingContact ->
    "You have not entered a phone number in your profile yet. Please provide a \
     phone number so that the recruitment team can contact you."
;;

let confirmable_to_string confirmable =
  (match confirmable with
   | CancelAssignment -> "assignment", "cancel", None
   | CancelAssignmentWithFollowUps ->
     ( "assignment"
     , "cancel"
     , Some "Assignments to follow-up sessions will be canceled as well." )
   | CancelSession -> "session", "cancel", None
   | CloseSession -> "session", "close", Some "This action cannot be undone."
   | DeleteCustomField -> "field", "delete", None
   | DeleteCustomFieldOption -> "option", "delete", None
   | DeleteEmailSuffix -> "email suffix", "delete", None
   | DeleteExperiment -> "experiment", "delete", None
   | DeleteExperimentFilter -> "filter", "delete", None
   | DeleteFile -> " file", "delete", None
   | DeleteGtxApiKey ->
     ( "GTX Api key"
     , "delete"
     , Some
         "Text messages can no longer be sent without a GTX Api key. This \
          action will disable any sending of text messages." )
   | DeleteMailing -> "mailing", "delete", None
   | DeleteMessageTemplate -> "message template", "delete", None
   | DeleteSession -> "session", "delete", None
   | DeleteSmtpServer -> "email Server", "delete", None
   | LoadDefaultTemplate ->
     "default template", "load", Some "The current content is overwritten."
   | MarkAssignmentAsDeleted -> "assignment as deleted", "mark", None
   | MarkAssignmentWithFollowUpsAsDeleted ->
     ( "assignment as deleted"
     , "mark"
     , Some
         "Assignments to follow-up sessions will be marked as deleted as well."
     )
   | PauseAccount -> "account", "pause", None
   | PromoteContact ->
     ( "contact"
     , "promote"
     , Some
         "The contact will no longer be invited for experiments and can no \
          longer register for them." )
   | PublishCustomField ->
     ( "field an all associated options"
     , "publish"
     , Some "You will not be able to delete it field anymore." )
   | PublishCustomFieldOption ->
     "option", "publish", Some "You will not be able to delete the it anymore."
   | ReactivateAccount -> "account", "reactivate", None
   | RemoveRule -> "rule", "delete", None
   | RemoveTag -> "tag", "remove", None
   | RescheduleSession -> "session", "reschedule", None
   | ResetInvitations ->
     ( "invitations"
     , "reset"
     , Some
         "Subsequently, all previous invitations up to the now are ignored, \
          i.e. contacts who have already been invited will receive a further \
          invitation." )
   | RevokeRole -> "role", "revoke", None
   | StopMailing -> "mailing", "stop", None)
  |> fun (obj, action, additive) ->
  Format.asprintf "Are you sure you want to %s the %s?" action obj
  |> fun msg ->
  additive |> CCOption.map_or ~default:msg (Format.asprintf "%s %s" msg)
;;
