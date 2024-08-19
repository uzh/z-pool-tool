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
  | AssignmentAlreadySubmitted
  | AssignmentIsCanceled
  | AssignmentIsClosed
  | AssignmentsHaveErrors
  | Authorization of string
  | CannotBeDeleted of Field.t
  | CannotBeUpdated of Field.t
  | CaqtiError of string
  | Connection of string
  | Conformist of (Field.t * t) list
  | ConformistModuleErrorType
  | ContactDoesNotMatchFilter
  | ContactExperimentNotFound
  | ContactIsInactive
  | ContactSignupInvalidEmail
  | ContactUnconfirmed
  | CustomFieldNoOptions
  | CustomFieldTypeChangeNotAllowed
  | DatabaseAddPoolFirst
  | Decode of Field.t
  | DecodeAction
  | DefaultMustNotBeUnchecked
  | DeleteContactUpcomingSessions
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
  | JobCannotBeRetriggered
  | JobPending
  | LoginProvideDetails
  | MaxLength of int
  | MeantimeUpdate of Field.t
  | MigrationFailed of string
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
  | SessionOverlap
  | SessionRegistrationViaParent
  | SessionTenantNotFound
  | Smaller of (Field.t * Field.t)
  | SmtpException of string
  | SmtpLoginMissingCredentials
  | TerminatoryRootError
  | TerminatoryRootErrorTitle
  | TerminatoryTenantError
  | TerminatoryTenantErrorTitle
  | TermsAndConditionsMissing
  | TermsAndConditionsNotAccepted
  | TextLengthMax of int
  | TextLengthMin of int
  | TextMessageError of string
  | TextMessageInterceptionError of string
  | TextMessageDlrAlreadyReceived
  | TimeInPast
  | TimeSpanPositive
  | TokenAlreadyUsed
  | TokenInvalidFormat
  | TooShort
  | Undefined of Field.t
  | Uniqueness of Field.t
  | Unsupported of string
  | WriteOnlyModel
[@@deriving eq, show, yojson, variants, sexp_of]

exception Exn of t

let error_to_exn t = Failure (show t)
let get_or_failwith error = CCResult.(error |> map_err show |> get_or_failwith)
