open Sexplib.Conv
open Ppx_yojson_conv_lib.Yojson_conv

type t =
  | AddedToWaitingList
  | AssignmentCreated
  | Canceled of Field.t
  | CellPhoneTokenSent
  | CellPhoneVerified
  | Closed of Field.t
  | ContactPromoted
  | Created of Field.t
  | Deleted of Field.t
  | EmailConfirmationMessage
  | EmailUpdateConfirmationMessage
  | EmailVerified
  | FileDeleted
  | ImportCompleted
  | MarkedAsDeleted of Field.t
  | PasswordChanged
  | PasswordReset
  | PasswordResetSuccessMessage
  | PausedToggled of bool
  | Published of Field.t
  | RemindersResent
  | RemovedFromWaitingList
  | Rescheduled of Field.t
  | Resent of Field.t
  | ResetInvitations
  | RoleAssigned
  | RoleUnassigned
  | Sent of Field.t
  | SentList of Field.t
  | SettingsUpdated
  | SmtpConfigurationAdded
  | SmtpDetailsUpdated
  | SmtpPasswordUpdated
  | Stopped of Field.t
  | TagAssigned
  | TagRemoved
  | TenantUpdateDatabase
  | TenantUpdateDetails
  | Updated of Field.t
  | Validated of Field.t
  | VerificationMessageResent
[@@deriving eq, show, yojson, variants, sexp_of]
