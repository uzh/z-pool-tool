type t =
  | DashboardTitle
  | EmailConfirmationNote
  | EmailConfirmationTitle
  | EmtpyList of Entity_message.Field.t
  | ExperimentContactEnrolledNote
  | ExperimentListTitle
  | ExperimentNewTitle
  | ExperimentSessionReminderHint
  | ExperimentWaitingListTitle
  | Files
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
  | MailingNewTitle
  | NoEntries of Entity_message.Field.t
  | ProfileCompletionTitle
  | RateTotalSent of int
  | Reminder
  | ResetPasswordLink
  | ResetPasswordTitle
  | SessionDetailTitle of Ptime.t
  | SessionIndent
  | SessionReminder
  | SessionReminderDefaultLeadTime of Entity.Reminder.LeadTime.t
  | SessionReminderDefaultSubject of Entity.Reminder.Subject.t
  | SessionReminderDefaultText of Entity.Reminder.Text.t
  | SessionSignUpTitle
  | SignUpAcceptTermsAndConditions
  | SignUpTitle
  | SwitchChronological
  | SwitchGrouped
  | TermsAndConditionsTitle
  | TextTemplates
  | UserProfileDetailsSubtitle
  | UserProfileLoginSubtitle
  | UserProfilePausedNote
  | UserProfileTitle
  | Validation
  | WaitingListIsDisabled

type nav_link =
  | Admins
  | Assignments
  | Contacts
  | CustomFields
  | Dashboard
  | Experiments
  | I18n
  | Invitations
  | Locations
  | LoginInformation
  | Login
  | Logout
  | Mailings
  | Overview
  | PersonalDetails
  | Profile
  | Sessions
  | Settings
  | Tenants
  | WaitingList
[@@deriving eq]

type hint =
  | AllowUninvitedSignup
  | AssignContactFromWaitingList
  | CustomFieldAdminViewOnly
  | CustomHtmx of string
  | DirectRegistrationDisbled
  | Distribution
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
  | SessionReminderLanguageHint
  | SignUpForWaitingList
  | TemplateTextElementsHint
  | TimeSpanPickerHint

type confirmable =
  | CancelSession
  | DeleteCustomFieldOption
  | DeleteEmailSuffix
  | DeleteExperiment
  | DeleteFile
  | DeleteMailing
  | DeleteSession
  | StopMailing
