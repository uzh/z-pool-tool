type t =
  | Address
  | AvailableSpots
  | Canceled
  | Closed
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
  | ExperimentListEmpty
  | ExperimentListPublicTitle
  | ExperimentListTitle
  | ExperimentNewTitle
  | ExperimentSessionReminderHint
  | ExperimentWaitingListTitle
  | Files
  | FilterNrOfContacts
  | FollowUpSessionFor
  | HomeTitle
  | I18nTitle
  | ImportConfirmationNote
  | ImportConfirmationTitle
  | ImportPendingNote
  | ImportPendingTitle
  | LocationFileNew
  | LocationListTitle
  | LocationNewTitle
  | LocationNoFiles
  | LocationNoSessions
  | LoginTitle
  | MailingDetailTitle of Ptime.t
  | MailingExperimentSessionFullyBooked
  | MailingNewTitle
  | ExperimentMessagingSubtitle
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
  | SelectedTags
  | SelectedTagsEmpty
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
  | PastExperimentListPublicTitle
  | PastSessionsTitle
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
  | ParticipationTags
  | PersonalDetails
  | Profile
  | Queue
  | Rules
  | Schedules
  | Sessions
  | Settings
  | Smtp
  | SystemSettings
  | Tags
  | Tenants
  | Users
  | WaitingList
[@@deriving eq]

type hint =
  | AllowUninvitedSignup
  | AssignContactFromWaitingList
  | AssignmentsMarkedAsClosed
  | ContactCurrentCellPhone of string
  | ContactNoCellPhone
  | ContactEnterCellPhoneToken of string
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
  | ExperimentContactPerson
  | ExperimentMailings
  | ExperimentMailingsRegistrationDisabled
  | ExperimentSessions
  | ExperimentSessionsPublic
  | ExperimentWaitingList
  | FilterContacts
  | TestPhoneNumber
  | I18nText of string
  | LocationFiles
  | Locations
  | LocationSessions
  | MissingMessageTemplates of string * string list
  | NumberIsDaysHint
  | NumberIsSecondsHint
  | NumberIsWeeksHint
  | Overbook
  | ParticipationTags
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
  | SessionCloseParticipationTagsSelected
  | SessionCloseNoParticipationTagsSelected
  | SessionCloseHints
  | SessionCloseLegend
  | SessionRegistrationFollowUpHint
  | SessionRegistrationHint
  | SessionReminderLanguageHint
  | SignUpForWaitingList
  | SmtpSettingsDefaultFlag
  | SmtpSettingsIntro
  | TagsIntro
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
  | RemoveTag
  | RevokeRole
  | StopMailing
