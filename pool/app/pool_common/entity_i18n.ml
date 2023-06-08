type t =
  | Address
  | AvailableSpots
  | Canceled
  | ContactWaitingListEmpty
  | ContactWaitingListTitle
  | DashboardProfileCompletionText
  | DashboardProfileCompletionTitle
  | DashboardTitle
  | DeletedAssignments
  | DontHaveAnAccount
  | EmailConfirmationNote
  | EmailConfirmationTitle
  | EmtpyList of Entity_message.Field.t
  | ExperimentContactEnrolledNote
  | ExperimentListTitle
  | ExperimentListEmpty
  | ExperimentListPublicTitle
  | ExperimentNewTitle
  | ExperimentSessionReminderHint
  | ExperimentWaitingListTitle
  | Files
  | FilterNrOfContacts
  | FollowUpSessionFor
  | HomeTitle
  | I18nTitle
  | LocationFileNew
  | LocationListTitle
  | LocationNewTitle
  | LocationNoFiles
  | LocationNoSessions
  | LoginTitle
  | MailingDetailTitle of Ptime.t
  | MailingExperimentSessionFullyBooked
  | MailingNewTitle
  | NoEntries of Entity_message.Field.t
  | OurPartners
  | ProfileCompletionText
  | RateTotalSent of int
  | Reminder
  | ResetPasswordLink
  | ResetPasswordTitle
  | RoleApplicableToAssign
  | RoleCurrentlyAssigned
  | RoleCurrentlyNoneAssigned of Entity_message.Field.t
  | RolesGranted
  | SentInvitations
  | SessionDetailTitle of Ptime.t
  | SessionIndent
  | SessionReminder
  | SessionReminderDefaultLeadTime of Entity.Reminder.LeadTime.t
  | SessionRegistrationTitle
  | SignUpAcceptTermsAndConditions
  | SignUpCTA
  | SignUpTitle
  | SortUngroupedFields
  | SwitchChronological
  | SwitchGrouped
  | TermsAndConditionsTitle
  | TextTemplates
  | UpcomingSessionsListEmpty
  | UpcomingSessionsTitle
  | UserProfileDetailsSubtitle
  | UserProfileLoginSubtitle
  | UserProfilePausedNote
  | Validation
  | WaitingListIsDisabled

type nav_link =
  | Admins
  | Assignments
  | ContactInformation
  | Contacts
  | CustomFields
  | Dashboard
  | Experiments
  | Field of Entity_message.Field.t
  | Filter
  | I18n
  | Invitations
  | Locations
  | Login
  | LoginInformation
  | Logout
  | Mailings
  | MessageTemplates
  | OrganisationalUnits
  | Overview
  | PersonalDetails
  | Profile
  | Queue
  | Rules
  | Schedules
  | Sessions
  | Settings
  | Smtp
  | SystemSettings
  | Tenants
  | Users
  | WaitingList
[@@deriving eq]

type hint =
  | AllowUninvitedSignup
  | AssignContactFromWaitingList
  | AssignmentsMarkedAsClosed
  | ContactCurrentPhoneNumber of string
  | ContactNoPhoneNumber
  | ContactEnterPhoneNumberToken of string
  | ContactPhoneNumberVerificationWasReset
  | ContactOnWaitingList
  | ContactProfileVisibleOverride
  | CustomFieldAdminInputOnly
  | CustomFieldAdminOverride
  | CustomFieldAdminOverrideUpdate
  | CustomFieldAdminViewOnly
  | CustomFieldContactModel
  | CustomFieldExperimentModel
  | CustomFieldGroups
  | CustomFieldNoContactValue
  | CustomFieldOptionsCompleteness
  | CustomFieldSessionModel
  | CustomFieldSort of Entity_message.Field.t
  | CustomFieldTypeText
  | CustomFieldTypeSelect
  | CustomFieldTypeMultiSelect
  | CustomHtmx of string
  | DirectRegistrationDisbled
  | Distribution
  | EmailPlainText
  | ExperimentAssignment
  | ExperimentMailings
  | ExperimentSessions
  | ExperimentSessionsPublic
  | ExperimentWaitingList
  | FilterContacts
  | I18nText of string
  | LocationFiles
  | Locations
  | LocationSessions
  | MissingMessageTemplates of string * string list
  | NumberIsDaysHint
  | NumberIsSecondsHint
  | NumberIsWeeksHint
  | Overbook
  | Rate
  | RateDependencyWith
  | RateDependencyWithout
  | RateNumberPerMinutes of int * float
  | RegistrationDisabled
  | RulesIntro
  | ScheduleEvery of Ptime.Span.t
  | ScheduleAt of Ptime.t
  | ScheduledIntro
  | SearchByFields of Entity_message.Field.t list
  | SelectedDateIsPast
  | SessionCancellationMessageFollowUps
  | SessionCancellationWithFollowups
  | SessionCancelMessage
  | SessionClose
  | SessionRegistrationFollowUpHint
  | SessionRegistrationHint
  | SessionReminderLanguageHint
  | SignUpForWaitingList
  | SmtpSettingsIntro
  | TemplateTextElementsHint
  | TimeSpanPickerHint
  | WaitingListPhoneMissingContact

type confirmable =
  | CancelAssignment
  | CancelAssignmentWithFollowUps
  | CancelSession
  | DeleteCustomField
  | DeleteCustomFieldOption
  | DeleteEmailSuffix
  | DeleteExperiment
  | DeleteExperimentFilter
  | DeleteFile
  | DeleteMailing
  | DeleteSession
  | MarkAssignmentAsDeleted
  | MarkAssignmentWithFollowUpsAsDeleted
  | PublisCustomField
  | PublisCustomFieldOption
  | RemoveRule
  | RevokeRole
  | StopMailing
