type t =
  | DashboardTitle
  | EmailConfirmationNote
  | EmailConfirmationTitle
  | EmtpyList of Entity_message.Field.t
  | ExperimentListTitle
  | ExperimentWaitingListTitle
  | ExperimentContactEnrolledNote
  | HomeTitle
  | I18nTitle
  | LocationListTitle
  | LocationNewTitle
  | LocationNoSessions
  | LocationFileNew
  | LoginTitle
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
  | NumberIsDaysHint
  | NumberIsWeeksHint
  | Overbook
  | RegistrationDisabled
  | SignUpForWaitingList
