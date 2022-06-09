type t =
  | DashboardTitle
  | EmailConfirmationNote
  | EmailConfirmationTitle
  | EmtpyList of Entity_message.Field.t
  | ExperimentListTitle
  | ExperimentWaitingListTitle
  | ExperimentContactEnrolledNote
  | Files
  | HomeTitle
  | I18nTitle
  | LocationListTitle
  | LocationNewTitle
  | LocationNoFiles
  | LocationNoSessions
  | LocationFileNew
  | LoginTitle
  | NumberIsDaysHint
  | NumberIsWeeksHint
  | RateDependencyHint
  | RateHint
  | RateNumberPerMinutesHint of int
  | RateTotalSent
  | ResetPasswordLink
  | ResetPasswordTitle
  | SessionDetailTitle of Ptime.t
  | SessionSignUpTitle
  | SignUpAcceptTermsAndConditions
  | SignUpTitle
  | TermsAndConditionsTitle
  | UserProfileLoginSubtitle
  | UserProfileDetailsSubtitle
  | UserProfileTitle
  | UserProfilePausedNote
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
  | NumberIsSecondsHint
  | NumberIsDaysHint
  | NumberIsWeeksHint
  | Overbook
  | RegistrationDisabled
  | SignUpForWaitingList

type confirmable =
  | CancelSession
  | DeleteEmailSuffix
  | DeleteExperiment
  | DeleteFile
  | DeleteSession
