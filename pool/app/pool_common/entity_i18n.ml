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
  | CustomFieldsSettings
  | DashboardProfileCompletionText
  | DashboardProfileCompletionTitle
  | DashboardTitle
  | DeletedAssignments
  | Disabled
  | DontHaveAnAccount
  | EmailConfirmationNote
  | EmailConfirmationTitle
  | EmptyListGeneric
  | EmtpyList of Entity_message.Field.t
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
  | JobCloneOf
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
  | MessageHistory of string
  | NoEntries of Entity_message.Field.t
  | Note
  | OurPartners
  | Past
  | PastExperimentListPublicTitle
  | PastSessionsTitle
  | PoolStatistics
  | ProfileCompletionText
  | Reminder
  | ResendReminders
  | ResetPasswordLink
  | ResetPasswordTitle
  | RoleApplicableToAssign
  | RoleCurrentlyAssigned
  | RoleCurrentlyNoneAssigned of Entity_message.Field.t
  | RolesGranted
  | SelectedTags
  | SelectedTagsEmpty
  | SentInvitations
  | SessionCloseScreen
  | SessionDetailTitle of Ptime.t
  | SessionIndent
  | SessionRegistrationTitle
  | SessionReminder
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
  | MessageHistory
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
  | TextMessages
  | Users
  | WaitingList
[@@deriving eq]

type hint =
  | AdminOverwriteContactValues
  | AllowUninvitedSignup
  | AssignContactFromWaitingList
  | AssignmentConfirmationMessageFollowUps
  | AssignmentsMarkedAsClosed
  | ContactCurrentCellPhone of string
  | ContactEnrollmentDoesNotMatchFilter
  | ContactEnrollmentRegistrationDisabled
  | ContactEnterCellPhoneToken of string
  | ContactLanguage
  | ContactNoCellPhone
  | ContactOnWaitingList
  | ContactPhoneNumberVerificationWasReset
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
  | CustomFieldTypeMultiSelect
  | CustomFieldTypeSelect
  | CustomFieldTypeText
  | CustomHtmx of string
  | DefaultReminderLeadTime of Ptime.Span.t
  | DirectRegistrationDisbled
  | Distribution
  | EmailPlainText
  | ExperimentAssignment
  | ExperimentContactPerson
  | ExperimentLanguage
  | ExperimentMailings
  | ExperimentMailingsRegistrationDisabled
  | ExperimentMessageTemplates
  | ExperimentSessions
  | ExperimentSessionsPublic
  | ExperimentWaitingList
  | ExternalDataRequired
  | GtxKeyMissing
  | GtxKeyStored
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
  | ScheduleAt of Ptime.t
  | ScheduledIntro
  | ScheduleEvery of Ptime.Span.t
  | SearchByFields of Entity_message.Field.t list
  | SelectedDateIsPast
  | SelectedOptionsCountMax of int
  | SelectedOptionsCountMin of int
  | SessionCancellationMessageFollowUps
  | SessionCancellationWithFollowups
  | SessionCancelMessage
  | SessionCloseHints
  | SessionCloseLegendNoShow
  | SessionCloseLegendParticipated
  | SessionCloseNoParticipationTagsSelected
  | SessionCloseParticipationTagsSelected
  | SessionRegistrationFollowUpHint
  | SessionRegistrationHint
  | SessionReminderLanguageHint
  | SessionReminderLeadTime
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
  | TestPhoneNumber
  | TextLengthMax of int
  | TextLengthMin of int
  | UserImportInterval
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
  | DeleteGtxApiKey
  | DeleteMailing
  | DeleteMessageTemplate
  | DeleteSession
  | DeleteSmtpServer
  | LoadDefaultTemplate
  | MarkAssignmentAsDeleted
  | MarkAssignmentWithFollowUpsAsDeleted
  | PauseAccount
  | PromoteContact
  | PublishCustomField
  | PublishCustomFieldOption
  | ReactivateAccount
  | RemoveRule
  | RemoveTag
  | RescheduleSession
  | ResetInvitations
  | RevokeRole
  | StopMailing
