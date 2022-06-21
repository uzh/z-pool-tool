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
  | RateTotalSent of int
  | Reminder
  | ResetPasswordLink
  | ResetPasswordTitle
  | SessionDetailTitle of Ptime.t
  | SessionReminder
  | SessionReminderDefaultLeadTime of Entity.Reminder.LeadTime.t
  | SessionReminderDefaultText of Entity.Reminder.Text.t
  | SessionSignUpTitle
  | SignUpAcceptTermsAndConditions
  | SignUpTitle
  | TermsAndConditionsTitle
  | UserProfileDetailsSubtitle
  | UserProfileLoginSubtitle
  | UserProfilePausedNote
  | UserProfileTitle
  | WaitingListIsDisabled

type nav_link =
  | Assignments
  | Dashboard
  | Experiments
  | I18n
  | Invitations
  | Locations
  | LoginInformation
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
  | AssignContactFromWaitingList
  | DirectRegistrationDisbled
  | Distribution
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
  | TimeSpanPickerHint

type confirmable =
  | CancelSession
  | DeleteEmailSuffix
  | DeleteExperiment
  | DeleteFile
  | DeleteMailing
  | DeleteSession
  | StopMailing
