type t =
  | DashboardTitle
  | EmailConfirmationNote
  | EmailConfirmationTitle
  | ExperimentContactEnrolledNote
  | ExperimentListTitle
  | ExperimentNewTitle
  | ExperimentWaitingListTitle
  | HomeTitle
  | I18nTitle
  | InvitationListTitle
  | InvitationNewTitle
  | LoginTitle
  | NoEntries of Entity_message.Field.t
  | NumberIsDaysHint
  | NumberIsWeeksHint
  | ResetPasswordLink
  | ResetPasswordTitle
  | SessionListTitle
  | SessionNewTitle
  | SessionSignUpTitle
  | SessionUpdateTitle
  | SignUpAcceptTermsAndConditions
  | SignUpTitle
  | TermsAndConditionsTitle
  | UserProfileDetailsSubtitle
  | UserProfileLoginSubtitle
  | UserProfilePausedNote
  | UserProfileTitle

type nav_link =
  | Dashboard
  | Experiments
  | I18n
  | Profile
  | Invitations
  | Sessions
  | Settings
  | Tenants
  | WaitingList
