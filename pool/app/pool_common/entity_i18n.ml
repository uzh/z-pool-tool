type t =
  | Activity
  | Address
  | AdminComment
  | AnnouncementsListTitle
  | AnnouncementsTenantSelect
  | AssignmentEditTagsWarning
  | AssignmentListEmpty
  | AvailableSpots
  | Canceled
  | CanceledSessionsTitle
  | Closed
  | ContactWaitingListEmpty
  | CustomFieldsSettings
  | CustomFieldsSettingsCloseScreen
  | CustomFieldsSettingsDetailScreen
  | DashboardProfileCompletionText
  | DashboardProfileCompletionTitle
  | DashboardTitle
  | DeletedAssignments
  | Disabled
  | DontHaveAnAccount
  | EmailConfirmationNote
  | EmailConfirmationTitle
  | EmptyListGeneric
  | EmtpyList of Pool_message.Field.t
  | EnrollInExperiment
  | ExperimentHistory
  | ExperimentListEmpty
  | ExperimentListPublicTitle
  | ExperimentOnlineListEmpty
  | ExperimentOnlineListPublicTitle
  | ExperimentOnlineParticiated of Ptime.t
  | ExperimentOnlineParticipationDeadline of Ptime.t
  | ExperimentOnlineParticipationNoUpcoming
  | ExperimentOnlineParticipationUpcoming of Ptime.t
  | ExperimentListTitle
  | ExperimentMessagingSubtitle
  | ExperimentNewTitle
  | ExperimentSessionReminderHint
  | ExperimentStatistics
  | ExperimentWaitingListTitle
  | Files
  | FilterContactsDescription
  | FilterNrOfContacts
  | FilterNrOfSentInvitations
  | FollowUpSessionFor
  | Help
  | ImportConfirmationNote
  | ImportConfirmationTitle
  | ImportPendingNote
  | ImportPendingTitle
  | IncompleteSessions
  | InvitationsStatistics
  | InvitationsStatisticsIntro
  | Iteration
  | JobCloneOf
  | LocationDetails
  | LocationFileNew
  | LocationListTitle
  | LocationNewTitle
  | LocationNoFiles
  | LocationNoSessions
  | LocationStatistics
  | LoginTitle
  | MailingDetailTitle of Ptime.t
  | MailingDistributionDescription
  | MailingExperimentNoUpcomingSession
  | MailingExperimentNoUpcomingTimewindow
  | MailingExperimentSessionFullyBooked
  | MailingNewTitle
  | MessageHistory of string
  | NoEntries of Pool_message.Field.t
  | NoInvitationsSent
  | Note
  | NotMatchingFilter
  | OurPartners
  | Past
  | PastSessionsTitle
  | PoolStatistics
  | ProfileCompletionText
  | Reminder
  | ResendReminders
  | ResetPasswordLink
  | ResetPasswordTitle
  | RoleApplicableToAssign
  | RoleCurrentlyAssigned
  | RoleCurrentlyNoneAssigned of Pool_message.Field.t
  | RolesGranted
  | SelectedTags
  | SelectedTagsEmpty
  | SessionCloseScreen
  | SessionDetailScreen
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
  | System
  | TermsAndConditionsLastUpdated of Ptime.t
  | TermsAndConditionsTitle
  | TermsAndConditionsUpdated
  | TenantMaintenanceText
  | TenantMaintenanceTitle
  | TextTemplates
  | TimeWindowDetailTitle of string
  | UpcomingSessionsListEmpty
  | UpcomingSessionsTitle
  | UserProfileDetailsSubtitle
  | UserProfileLoginSubtitle
  | UserProfilePausedNote
  | Validation
  | VersionsListTitle
  | WaitingListIsDisabled

type nav_link =
  | ActorPermissions
  | Admins
  | Announcements
  | Assignments
  | ContactInformation
  | Contacts
  | Credits
  | CustomFields
  | Dashboard
  | Experiments
  | ExperimentsCustom of string
  | ExternalDataIds
  | Field of Pool_message.Field.t
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
  | QueueHistory
  | RolePermissions
  | Schedules
  | SentInvitations
  | Sessions
  | Settings
  | Smtp
  | SignupCodes
  | SystemSettings
  | Tags
  | Tenants
  | TextMessages
  | TimeWindows
  | Users
  | WaitingList
  | Versions
[@@deriving eq]

type hint =
  | AdminOverwriteContactValues
  | AllowUninvitedSignup
  | AssignContactFromWaitingList
  | AssignmentCancellationMessageFollowUps
  | AssignmentConfirmationMessageFollowUps
  | AssignmentsMarkedAsClosed
  | AssignmentsNotMatchingFilerSession of int
  | AssignmentWithoutSession
  | ContactCurrentCellPhone of string
  | ContactEnrollmentDoesNotMatchFilter
  | ContactEnrollmentRegistrationDisabled
  | ContactEnterCellPhoneToken of string
  | ContactInformationEmailHint
  | ContactLanguage
  | ContactNoCellPhone
  | ContactOnWaitingList
  | ContactPhoneNumberVerificationWasReset
  | ContactProfileVisibleOverride
  | ContactsWithoutCellPhone
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
  | CustomFieldSort of Pool_message.Field.t
  | CustomFieldTypeMultiSelect
  | CustomFieldTypeSelect
  | CustomFieldTypeText
  | CustomHtmx of string
  | DefaultReminderLeadTime of Ptime.Span.t
  | DeleteContact
  | DirectRegistrationDisbled
  | Distribution
  | DuplicateSession
  | DuplicateSessionList
  | EmailPlainText
  | ExperimentAssignment
  | ExperimentCallbackUrl
  | ExperimentContactPerson of string
  | ExperimentLanguage
  | ExperimentMailings
  | ExperimentMailingsRegistrationDisabled
  | ExperimentMessageTemplates
  | ExperimentSessions
  | ExperimentSessionsCancelDelete
  | ExperimentSessionsPublic
  | ExperimentSmtp of string
  | ExperimentStatisticsRegistrationPossible
  | ExperimentStatisticsSendingInvitations
  | ExperimentWaitingList
  | ExperimentSurveyRedirectUrl
  | ExperimentSurveyUrl
  | ExternalDataRequired
  | FileUploadAcceptMime of string list
  | FilterTemplates
  | GtxKeyMissing
  | GtxKeyStored
  | GtxSender
  | I18nText of string
  | LocationFiles
  | LocationsIndex
  | MailingLimit
  | MailingLimitExceedsMatchingContacts
  | MessageTemplateAccountSuspensionNotification
  | MessageTemplateAssignmentCancellation
  | MessageTemplateAssignmentConfirmation
  | MessageTemplateAssignmentSessionChange
  | MessageTemplateContactEmailChangeAttempt
  | MessageTemplateContactRegistrationAttempt
  | MessageTemplateEmailVerification
  | MessageTemplateExperimentInvitation
  | MessageTemplateManualSessionMessage
  | MessageTemplateMatcherNotification
  | MessageTemplateMatchFilterUpdateNotification
  | MessageTemplatePasswordChange
  | MessageTemplatePasswordReset
  | MessageTemplatePhoneVerification
  | MessageTemplateProfileUpdateTrigger
  | MessageTemplateSessionCancellation
  | MessageTemplateSessionReminder
  | MessageTemplateSessionReschedule
  | MessageTemplateSignupVerification
  | MessageTemplateTextTemplates
  | MessageTemplateUserImport
  | MessageTemplateWaitingListConfirmation
  | MissingMessageTemplates
  | NumberIsDaysHint
  | NumberIsSecondsHint
  | NumberIsWeeksHint
  | NumberMax of int
  | NumberMin of int
  | OnlineExperiment
  | Overbook
  | PartialUpdate
  | ParticipationTagsHint
  | PauseAccountAdmin
  | PauseAccountContact
  | Permissions
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
  | RoleIntro of Pool_message.Field.t * Pool_message.Field.t
  | RolePermissionsModelList
  | RolePermissionsRoleList
  | ScheduleAt of Ptime.t
  | ScheduledIntro
  | ScheduleEvery of Ptime.Span.t
  | SearchByFields of Pool_message.Field.t list
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
  | SignUpCodeHint
  | SignUpForWaitingList
  | SmtpSettingsDefaultFlag
  | SmtpSettingsIntro
  | SmtpValidation
  | SurveyUrl
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
  | DeleteContact
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
  | RemovePermission
  | RemoveRule
  | RemoveTag
  | RescheduleSession
  | ResetInvitations
  | RevokeRole
  | StopMailing
