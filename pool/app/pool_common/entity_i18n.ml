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
  | FilterNrOfSentInvitations
  | FollowUpSessionFor
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
  | Past
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
  | ContactEnrollmentDoesNotMatchFilter
  | ContactEnrollmentRegistrationDisabled
  | ContactNoCellPhone
  | ContactEnterCellPhoneToken of string
  | ContactLanguage
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
  | DefaultReminderLeadTime of Ptime.Span.t
  | DirectRegistrationDisbled
  | Distribution
  | EmailPlainText
  | ExperimentAssignment
  | ExperimentContactPerson
  | ExperimenterRole
  | ExperimentLanguage
  | ExperimentMailings
  | ExperimentMailingsRegistrationDisabled
  | ExperimentMessageTemplates
  | ExperimentSessions
  | ExperimentSessionsPublic
  | ExperimentWaitingList
  | ExternalDataRequired
  | TestPhoneNumber
  | I18nText of string
  | LocationFiles
  | LocationsIndex
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
  | MissingMessageTemplates
  | NumberIsDaysHint
  | NumberIsSecondsHint
  | NumberIsWeeksHint
  | NumberMax of int
  | NumberMin of int
  | Overbook
  | PartialUpdate
  | ParticipationTagsHint
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
  | SelectedOptionsCountMax of int
  | SelectedOptionsCountMin of int
  | SessionCancellationMessageFollowUps
  | SessionCancellationWithFollowups
  | SessionCancelMessage
  | SessionCloseParticipationTagsSelected
  | SessionCloseNoParticipationTagsSelected
  | SessionCloseHints
  | SessionCloseLegendNoShow
  | SessionCloseLegendParticipated
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
  | TenantUrl
  | TextLengthMin of int
  | TextLengthMax of int
  | TimeSpanPickerHint
  | WaitingListPhoneMissingContact
[@@deriving variants]

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
  | DeleteSmtpServer
  | LoadDefaultTemplate
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
