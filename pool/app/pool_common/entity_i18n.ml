type t =
  | DashboardTitle
  | EmailConfirmationNote
  | EmailConfirmationTitle
  | EmtpyList of Entity_message.Field.t
  | ExperimentContactEnrolledNote
  | ExperimentListTitle
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
  | RateTotalSent of int
  | ResetPasswordLink
  | ResetPasswordTitle
  | SessionDetailTitle of Ptime.t
  | SessionIndent
  | SessionSignUpTitle
  | SwitchChronological
  | SwitchGrouped
  | SignUpAcceptTermsAndConditions
  | SignUpTitle
  | TermsAndConditionsTitle
  | UserProfileDetailsSubtitle
  | UserProfileLoginSubtitle
  | UserProfilePausedNote
  | UserProfileTitle

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
