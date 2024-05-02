open Sexplib.Conv
open Ppx_yojson_conv_lib.Yojson_conv

type t =
  | AccessDenied
  | AccessDeniedMessage
  | AccountTemporarilySuspended of Utils.Ptime.t
  | AllLanguagesRequired of Field.t
  | AlreadyExisting of Field.t
  | AlreadyInPast
  | AlreadyInvitedToExperiment of string list
  | AlreadyPublished of Field.t
  | AlreadySignedUpForExperiment
  | AlreadyStarted
  | AssignmentIsCanceled
  | AssignmentIsClosed
  | AssignmentsHaveErrors
  | Authorization of string
  | CannotBeDeleted of Field.t
  | CannotBeUpdated of Field.t
  | CaqtiError of string
  | Conformist of (Field.t * t) list
  | ConformistModuleErrorType
  | ContactDoesNotMatchFilter
  | ContactExperimentNotFound
  | ContactIsInactive
  | ContactSignupInvalidEmail
  | ContactUnconfirmed
  | CustomFieldNoOptions
  | CustomFieldTypeChangeNotAllowed
  | Decode of Field.t
  | DecodeAction
  | DefaultMustNotBeUnchecked
  | DirectRegistrationIsDisabled
  | Disabled of Field.t
  | EmailAddressMissingAdmin
  | EmailAddressMissingRoot
  | EmailAlreadyInUse
  | EmailDeleteAlreadyVerified
  | EmailIdenticalToCurrent
  | EmailInterceptionError of string
  | EmailMalformed
  | EndBeforeStart
  | ExperimentSessionCountNotZero
  | FieldRequired of Field.t
  | FilterAndOrMustNotBeEmpty
  | FilterListValueMustNotBeEmpty
  | FilterMustNotContainTemplate
  | FollowUpIsEarlierThanMain
  | HtmxVersionNotFound of string
  | ImportPending
  | Invalid of Field.t
  | InvalidEmailSuffix of string list
  | InvalidHtmxRequest
  | InvalidJson of string
  | InvalidOptionSelected
  | InvalidPasswordHashingCount
  | InvalidRequest
  | IsMarkedAsDeleted of Field.t
  | JobPending
  | LoginProvideDetails
  | MeantimeUpdate of Field.t
  | Missing of Field.t
  | MutuallyExclusive of (Field.t * Field.t)
  | NegativeAmount
  | NoOptionSelected of Field.t
  | NotADatetime of (string * string)
  | NotANumber of string
  | NotEligible
  | NoTenantsRegistered
  | NotFound of Field.t
  | NotFoundList of Field.t * string list
  | NotHandled of string
  | NotInTimeRange
  | NoValue
  | NumberMax of int
  | NumberMin of int
  | Or of (t * t)
  | PasswordConfirmationDoesNotMatch
  | PasswordPolicyCapitalLetter
  | PasswordPolicyMinLength of int
  | PasswordPolicyNumber
  | PasswordPolicySpecialChar of char list
  | PasswordResetFailMessage
  | PasswordResetInvalidData
  | PermissionDeniedCreateRule
  | PermissionDeniedGrantRole
  | PermissionDeniedRevokeRole
  | PickMessageChannel
  | PoolContextNotFound
  | QueryNotCompatible of (Field.t * Field.t)
  | ReadOnlyModel
  | RegistrationDisabled
  | RequestRequiredFields
  | RequiredFieldsMissing
  | Retrieve of Field.t
  | SelectedOptionsCountMax of int
  | SelectedOptionsCountMin of int
  | SessionAlreadyCanceled of string
  | SessionAlreadyClosed of string
  | SessionFullyBooked
  | SessionHasAssignments
  | SessionHasFollowUps
  | SessionInPast
  | SessionInvalid
  | SessionNotClosed
  | SessionNotStarted
  | SessionRegistrationViaParent
  | SessionTenantNotFound
  | Smaller of (Field.t * Field.t)
  | SmtpException of string
  | TerminatoryRootError
  | TerminatoryRootErrorTitle
  | TerminatoryTenantError
  | TerminatoryTenantErrorTitle
  | TermsAndConditionsMissing
  | TermsAndConditionsNotAccepted
  | TextLengthMax of int
  | TextLengthMin of int
  | TextMessageInterceptionError of string
  | TimeInPast
  | TimeSpanPositive
  | TokenAlreadyUsed
  | TokenInvalidFormat
  | TooShort
  | Undefined of Field.t
  | Uniqueness of Field.t
  | WriteOnlyModel
[@@deriving eq, show, yojson, variants, sexp_of]

let error_to_exn t = Failure (show t)
let get_or_failwith error = CCResult.(error |> map_err show |> get_or_failwith)