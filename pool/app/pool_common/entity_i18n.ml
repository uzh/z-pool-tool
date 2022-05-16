type t =
  | DashboardTitle
  | EmailConfirmationNote
  | EmailConfirmationTitle
  | ExperimentNewTitle
  | ExperimentListTitle
  | ExperimentWaitingListTitle
  | HomeTitle
  | I18nTitle
  | InvitationListTitle
  | InvitationNewTitle
  | LoginTitle
  | ResetPasswordLink
  | ResetPasswordTitle
  | SessionListTitle
  | SessionNewTitle
  | SessionUpdateTitle
  | SessionSignUpTitle
  | SignUpAcceptTermsAndConditions
  | SignUpTitle
  | TermsAndConditionsTitle
  | UserProfileLoginSubtitle
  | UserProfileDetailsSubtitle
  | UserProfileTitle
  | UserProfilePausedNote

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
