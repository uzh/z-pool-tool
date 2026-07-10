open Sexplib.Conv

module Ptime = struct
  include Pool_model.Time

  module Span = struct
    include Pool_model.Time.Span
  end
end

type t =
  | ActivateAccountButton of string
  | ActivateAccountNote
  | ActivateAccountTitle
  | Activity
  | Address
  | AdminComment
  | AnnouncementsListTitle
  | AnnouncementsTenantSelect
  | ApiKeys
  | Assigned
  | AssignmentEditTagsWarning
  | AssignmentListEmpty
  | Available
  | AvailableSpots
  | Canceled
  | CanceledSessionsTitle
  | Closed
  | ConfirmAccountImport of string
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
  | FilterNrOfUnsuitableAssignments
  | FilterNuberMatchingUninvited
  | FollowUpSessionFor
  | HasGlobalRole of string
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
  | MatchesFilterChangeReasonFilter
  | MatchesFilterChangeReasonManually
  | MatchesFilterChangeReasonWorker
  | MessageHistory of string
  | NoEntries of Pool_message.Field.t
  | NoInvitationsSent
  | Note
  | NotMatchingFilter
  | OtpHint
  | OurPartners
  | Past
  | PastSessionsTitle
  | PoolStatistics
  | ProfileCompletionText
  | Reminder
  | ResendReminders
  | Reset
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
  | TenantOperatorExistingAdminText
  | TenantOperatorExistingAdminTitle
  | TenantOperatorExistingContactText
  | TenantOperatorExistingContactTitle
  | TextTemplates
  | TimeWindowDetailTitle of string
  | TotalSentInvitations
  | UpcomingSessionsListEmpty
  | UpcomingSessionsTitle
  | UserLoginBlockedUntil of Ptime.t
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
  | ApiKeys
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
  | ManageDuplicates
  | Mailings
  | MessageHistory
  | MessageTemplates
  | OrganisationalUnits
  | Overview
  | ParticipationTags
  | PersonalDetails
  | Pools
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
  | TextMessages
  | TimeWindows
  | Users
  | WaitingList
  | Versions
[@@deriving eq, show, sexp_of]

type hint =
  | AdminOverwriteContactValues
  | AllowUninvitedSignup
  | AssignContactFromWaitingList
  | AssignmentCancellationMessageFollowUps
  | AssignmentConfirmationMessageFollowUps
  | AssignmentsMarkedAsClosed
  | AssignmentsNotMatchingFilerSession of int
  | AssignmentWithoutSession
  | ContactAccountPaused
  | ContactCurrentCellPhone of string
  | ContactEnrollmentDoesNotMatchFilter
  | ContactEnrollmentRegistrationDisabled
  | ContactEnterCellPhoneToken of string
  | ContactExperimentNotMatchingFilter
  | ContactExperimentHistory
  | ContactInformationEmailHint
  | ContactLanguage
  | ContactCellPhoneUnverified
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
  | CustomFieldDuplicateWeight
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
  | DashboardDuplicateContactsNotification of int
  | DefaultReminderLeadTime of Ptime.Span.t
  | DeleteContact
  | DirectRegistrationDisbled
  | Distribution
  | DuplicateSession
  | DuplicateSessionList
  | EmailPlainText
  | EmailServiceFailingNotification of int
  | ExperimentAssignment
  | ExperimentCallbackUrl
  | ExperimentContactPerson of string
  | ExperimentLanguage
  | ExperimentMailings
  | ExperimentMailingsRegistrationDisabled
  | ExperimentMessageTemplates
  | ExperimentSessions
  | ExperimentTimewindows
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
  | PhoneVerificationHint
  | I18nText of string
  | LocationFiles
  | LocationsIndex
  | LoginTokenSent of string
  | MailingLimit
  | MailingLimitExceedsMatchingContacts
  | MergeContacts
  | MessageTemplateAccountSuspensionNotification
  | MessageTemplateAdminAccountCreated
  | MessageTemplateAssignmentCancellation
  | MessageTemplateAssignmentConfirmation
  | MessageTemplateAssignmentSessionChange
  | MessageTemplateContactEmailChangeAttempt
  | MessageTemplateContactRegistrationAttempt
  | MessageTemplateEmailVerification
  | MessageTemplateExperimentInvitation
  | MessageTemplateInactiveContactWarning
  | MessageTemplateInactiveContactDeactivation
  | MessageTemplateLogin2FAToken
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
  | MessageTemplateUserImportInactive
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
  | PermissionManage
  | PermissionsExplanationLink
  | ProfileOnly
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
  | ReleaseNotesHint of string
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
  | SessionCloseLegendVerified
  | SessionCloseNoParticipationTagsSelected
  | SessionCloseParticipationTagsSelected
  | SessionRegistrationFollowUpHint
  | SessionRegistrationHint
  | SessionReminderLanguageHint
  | SessionReminderLeadTime
  | SettigsInactiveUsers
  | SettingsContactEmail
  | SettingsNoEmailSuffixes
  | SettingsPageScripts
  | SignUpCodeHint
  | SignUpForWaitingList
  | SmtpMissing
  | SmtpSettingsDefaultFlag
  | SmtpSettingsInternalRegex
  | SmtpSettingsIntro
  | SmtpSettingsSystemAccountFlag
  | SmtpValidation
  | SurveyUrl
  | SwapSessions
  | TagsIntro
  | TemplateTextElementsHint
  | TenantDatabaseLabel
  | TenantDatabaseUrl
  | TenantMaintenanceFlag
  | TenantUrl
  | TestPhoneNumber
  | TextLengthMax of int
  | TextLengthMin of int
  | UnsubscribeExperimentInvitationsTitle
  | UnsubscribeExperimentInvitationsInfo of string
  | UserImportInterval
  | VerifyContact
  | WaitingListPhoneMissingContact
[@@deriving eq, show, variants, sexp_of]

type confirmable =
  | CancelAssignment
  | CancelAssignmentWithFollowUps
  | CancelSession
  | CloseSession
  | DeleteContact
  | DeleteCustomField
  | DeleteCustomFieldOption
  | DeleteExperiment
  | DeleteExperimentFilter
  | DeleteFile
  | DeleteGtxApiKey
  | DeleteMailing
  | DeleteMessageTemplate
  | DeleteSession
  | DeleteSmtpServer
  | DisableApiKey
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
  | UnsubscribeExperimentInvitation
