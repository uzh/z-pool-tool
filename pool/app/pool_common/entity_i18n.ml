type t =
  | DashboardTitle
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
  | NotifyVia
  | OurPartners
  | ProfileCompletionTitle
  | RateTotalSent of int
  | Reminder
  | ResetPasswordLink
  | ResetPasswordTitle
  | RoleApplicableToAssign
  | RoleCurrentlyAssigned
  | RoleCurrentlyNoneAssigned of Entity_message.Field.t
  | SentInvitations
  | SessionDetailTitle of Ptime.t
  | SessionIndent
  | SessionReminder
  | SessionReminderDefaultLeadTime of Entity.Reminder.LeadTime.t
  | SessionReminderDefaultSubject of Entity.Reminder.Subject.t
  | SessionReminderDefaultText of Entity.Reminder.Text.t
  | SessionRegistrationTitle
  | SignUpAcceptTermsAndConditions
  | SignUpCTA
  | SignUpTitle
  | SortUngroupedFields
  | SwitchChronological
  | SwitchGrouped
  | TermsAndConditionsTitle
  | TextTemplates
  | UserProfileDetailsSubtitle
  | UserProfileLoginSubtitle
  | UserProfilePausedNote
  | Validation
  | WaitingListIsDisabled

type nav_link =
  | Admins
  | Assignments
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
  | Overview
  | PersonalDetails
  | Profile
  | Sessions
  | Settings
  | SystemSettings
  | Tenants
  | Users
  | WaitingList
[@@deriving eq]

type hint =
  | AllowUninvitedSignup
  | AssignContactFromWaitingList
  | ContactOnWaitingList
  | CustomFieldAdminInputOnly
  | CustomFieldAdminViewOnly
  | CustomFieldContactModel
  | CustomFieldExperimentModel
  | CustomFieldSessionModel
  | CustomFieldGroups
  | CustomFieldSort of Entity_message.Field.t
  | CustomHtmx of string
  | DirectRegistrationDisbled
  | Distribution
  | ExperimentAssignment
  | ExperimentMailings
  | ExperimentWaitingList
  | ExperimentSessions
  | ExperimentSessionsPublic
  | LocationFiles
  | LocationSessions
  | Locations
  | I18nText of string
  | NumberIsDaysHint
  | NumberIsSecondsHint
  | NumberIsWeeksHint
  | Overbook
  | Rate
  | RateDependencyWith
  | RateDependencyWithout
  | RateNumberPerMinutes of int * float
  | RegistrationDisabled
  | SelectedDateIsPast
  | SessionCancelMessage
  | SessionClose
  | SessionReminderLanguageHint
  | SessionRegistrationHint
  | SignUpForWaitingList
  | TemplateTextElementsHint
  | TimeSpanPickerHint

type confirmable =
  | CancelAssignment
  | CancelSession
  | DeleteCustomField
  | DeleteCustomFieldOption
  | DeleteEmailSuffix
  | DeleteExperiment
  | DeleteFile
  | DeleteMailing
  | DeleteSession
  | PublisCustomField
  | PublisCustomFieldOption
  | StopMailing
