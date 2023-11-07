type t =
  | Activity
  | Address
  | AdminComment
  | AssignmentEditTagsWarning
  | AssignmentListEmpty
  | AvailableSpots
  | Canceled
  | Closed
  | ContactWaitingListEmpty
  | ContactWaitingListTitle
  | DashboardProfileCompletionText
  | DashboardProfileCompletionTitle
  | DashboardTitle
  | DeletedAssignments
  | Disabled
  | DontHaveAnAccount
  | EmailConfirmationNote
  | EmailConfirmationTitle
  | EmtpyList of Entity_message.Field.t
  | EmptyListGeneric
  | EnrollInExperiment
  | ExperimentListEmpty
  | ExperimentListPublicTitle
  | ExperimentListTitle
  | ExperimentMessagingSubtitle
  | ExperimentNewTitle
  | ExperimentSessionReminderHint
  | ExperimentWaitingListTitle
  | Files
  | FilterContactsDescription
  | FilterNrOfContacts
  | FollowUpSessionFor
  | HomeTitle
  | I18nTitle
  | ImportConfirmationNote
  | ImportConfirmationTitle
  | ImportPendingNote
  | ImportPendingTitle
  | InvitationsStatistics
  | InvitationsStatisticsIntro
  | LocationDetails
  | LocationFileNew
  | LocationListTitle
  | LocationNewTitle
  | LocationNoFiles
  | LocationNoSessions
  | LoginTitle
  | MailingDetailTitle of Ptime.t
  | MailingDistributionDescription
  | MailingExperimentSessionFullyBooked
  | MailingNewTitle
  | NoEntries of Entity_message.Field.t
  | Note
  | OurPartners
  | ProfileCompletionText
  | Reminder
  | ResendReminders
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
  | SessionRegistrationTitle
  | SignUpAcceptTermsAndConditions
  | SignUpCTA
  | SignUpTitle
  | SortUngroupedFields
  | SwapSessionsListEmpty
  | SwitchChronological
  | SwitchGrouped
  | TermsAndConditionsLastUpdated of Ptime.t
  | TermsAndConditionsTitle
  | TermsAndConditionsUpdated
  | TextTemplates
  | UpcomingSessionsListEmpty
  | PastExperimentListPublicTitle
  | PastSessionsTitle
  | PoolStatistics
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
  | Credits
  | CustomFields
  | Dashboard
  | Experiments
  | ExternalDataIds
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
  | PrivacyPolicy
  | Profile
  | Queue
  | RolePermissions
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
  | AdminOverwriteContactValues
  | AllowUninvitedSignup
  | AssignmentConfirmationMessageFollowUps
  | AssignContactFromWaitingList
  | AssignmentsMarkedAsClosed
  | AssistantRole
  | ContactCurrentCellPhone of string
  | ContactDoesNotMatchFilter
  | ContactNoCellPhone
  | ContactEnterCellPhoneToken of string
  | ContactPhoneNumberVerificationWasReset
  | ContactOnWaitingList
  | ContactProfileVisibleOverride
  | CustomFieldAdminInputOnly
  | CustomFieldAdminOverride
  | CustomFieldAdminOverrideUpdate
  | CustomFieldAdminViewOnly
  | CustomFieldAnsweredOnRegistration
  | CustomFieldContactModel
  | CustomFieldExperimentModel
  | CustomFieldGroups
  | CustomFieldNoContactValue
  | CustomFieldOptionsCompleteness
  | CustomFieldPromptOnRegistration
  | CustomFieldSessionModel
  | CustomFieldSort of Entity_message.Field.t
  | CustomFieldTypeText
  | CustomFieldTypeSelect
  | CustomFieldTypeMultiSelect
  | CustomHtmx of string
  | DefaultReminderLeadTime of Entity.Reminder.LeadTime.t
  | DirectRegistrationDisbled
  | Distribution
  | EmailPlainText
  | ExperimentAssignment
  | ExperimentContactPerson
  | ExperimenterRole
  | ExperimentMailings
  | ExperimentMailingsRegistrationDisabled
  | ExperimentSessions
  | ExperimentSessionsPublic
  | ExperimentWaitingList
  | ExternalDataRequired
  | TestPhoneNumber
  | I18nText of string
  | LocationFiles
  | Locations
  | LocationSessions
  | MailingLimit
  | MessageTemplateAccountSuspensionNotification
  | MessageTemplateAssignmentConfirmation
  | MessageTemplateAssignmentSessionChange
  | MessageTemplateContactEmailChangeAttempt
  | MessageTemplateContactRegistrationAttempt
  | MessageTemplateEmailVerification
  | MessageTemplateExperimentInvitation
  | MessageTemplatePasswordChange
  | MessageTemplatePasswordReset
  | MessageTemplatePhoneVerification
  | MessageTemplateProfileUpdateTrigger
  | MessageTemplateSessionCancellation
  | MessageTemplateSessionReminder
  | MessageTemplateSessionReschedule
  | MessageTemplateSignupVerification
  | MessageTemplateUserImport
  | MessageTemplateWaitingListConfirmation
  | MissingMessageTemplates of string * string list
  | NumberIsDaysHint
  | NumberIsSecondsHint
  | NumberIsWeeksHint
  | Overbook
  | PartialUpdate
  | ParticipationTags
  | PauseAccountAdmin
  | PauseAccountContact
  | PromoteContact
  | RateDependencyWith
  | RateDependencyWithout
  | RateNumberPerMinutes of int * float
  | RegistrationDisabled
  | RescheduleSession
  | ResendRemindersChannel
  | ResendRemindersWarning
  | ResetInvitations
  | ResetInvitationsLastReset of Ptime.t
  | RoleIntro of Entity_message.Field.t * Entity_message.Field.t
  | RolePermissionsIntro
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
  | SessionReminderLeadTime
  | SessionReminderLanguageHint
  | SettingsNoEmailSuffixes
  | SignUpForWaitingList
  | SmtpSettingsDefaultFlag
  | SmtpSettingsIntro
  | SwapSessions
  | TagsIntro
  | TemplateTextElementsHint
  | TenantDatabaseLabel
  | TenantDatabaseUrl
  | TimeSpanPickerHint
  | WaitingListPhoneMissingContact

type confirmable =
  | CancelAssignment
  | CancelAssignmentWithFollowUps
  | CancelSession
  | CloseSession
  | DeleteCustomField
  | DeleteCustomFieldOption
  | DeleteEmailSuffix
  | DeleteExperiment
  | DeleteExperimentFilter
  | DeleteFile
  | DeleteMailing
  | DeleteMessageTemplate
  | DeleteSession
  | MarkAssignmentAsDeleted
  | MarkAssignmentWithFollowUpsAsDeleted
  | PauseAccount
  | PromoteContact
  | PublisCustomField
  | PublisCustomFieldOption
  | ReactivateAccount
  | RemoveRule
  | RemoveTag
  | RescheduleSession
  | ResetInvitations
  | RevokeRole
  | StopMailing
