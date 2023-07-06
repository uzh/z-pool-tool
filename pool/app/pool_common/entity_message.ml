open Sexplib.Conv
open Ppx_yojson_conv_lib.Yojson_conv

module Ptime = struct
  include Ptime

  let t_of_yojson = Utils.Ptime.ptime_of_yojson
  let yojson_of_t = Utils.Ptime.yojson_of_ptime
  let sexp_of_t = Utils.Ptime.ptime_to_sexp
end

module Field = Entity_message_field

(* TODO [aerben] make these general, compare what fields exist already, whenever
   pattern is "FIELD_ADJECTIVE", turn FIELD to Field.t and make it ADJECTIVE of
   Field.t *)
type error =
  | AccountTemporarilySuspended of Ptime.t
  | AccessDenied
  | AccessDeniedMessage
  | AllLanguagesRequired of Field.t
  | AlreadyInPast
  | AlreadyInvitedToExperiment of string list
  | AlreadyPublished of Field.t
  | AlreadySignedUpForExperiment
  | AlreadyStarted
  | AssignmentIsCanceled
  | AssignmentIsClosed
  | Authorization of string
  | Conformist of (Field.t * error) list
  | ConformistModuleErrorType
  | ContactSignupInvalidEmail
  | ContactUnconfirmed
  | CustomFieldTypeChangeNotAllowed
  | Decode of Field.t
  | DecodeAction
  | DirectRegistrationIsDisabled
  | Disabled of Field.t
  | EmailAddressMissingAdmin
  | EmailAddressMissingRoot
  | EmailAlreadyInUse
  | EmailDeleteAlreadyVerified
  | EmailMalformed
  | EndBeforeStart
  | ExperimentSessionCountNotZero
  | FilterAndOrMustNotBeEmpty
  | FilterListValueMustNotBeEmpty
  | FilterMustNotContainTemplate
  | FollowUpIsEarlierThanMain
  | HtmxVersionNotFound of string
  | Invalid of Field.t
  | InvalidEmailSuffix of string list
  | InvalidJson of string
  | InvalidRequest
  | InvalidHtmxRequest
  | InvalidOptionSelected
  | IsMarkedAsDeleted of Field.t
  | LoginProvideDetails
  | MeantimeUpdate of Field.t
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
  | Or of (error * error)
  | PasswordConfirmationDoesNotMatch
  | PasswordPolicyMinLength of int
  | PasswordPolicyCapitalLetter
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
  | SessionAlreadyCanceled of string
  | SessionAlreadyClosed of string
  | SessionFullyBooked
  | SessionHasAssignments
  | SessionHasFollowUps
  | SessionInPast
  | SessionInvalid
  | SessionNotStarted
  | SessionRegistrationViaParent
  | SessionTenantNotFound
  | Smaller of (Field.t * Field.t)
  | TerminatoryRootError
  | TerminatoryRootErrorTitle
  | TerminatoryTenantError
  | TerminatoryTenantErrorTitle
  | TermsAndConditionsMissing
  | TermsAndConditionsNotAccepted
  | TextLengthMax of int
  | TextLengthMin of int
  | TimeInPast
  | TimeSpanPositive
  | TokenAlreadyUsed
  | TokenInvalidFormat
  | Undefined of Field.t
  | WriteOnlyModel
[@@deriving eq, show, yojson, variants, sexp_of]

type warning = Warning of string
[@@deriving eq, show, yojson, variants, sexp_of]

let error_to_exn error = Failure (show_error error)

type success =
  | AddedToWaitingList
  | AssignmentCreated
  | Canceled of Field.t
  | Closed of Field.t
  | Created of Field.t
  | Deleted of Field.t
  | EmailConfirmationMessage
  | EmailVerified
  | FileDeleted
  | MarkedAsDeleted of Field.t
  | PasswordChanged
  | PasswordReset
  | PasswordResetSuccessMessage
  | CellPhoneTokenSent
  | CellPhoneVerified
  | Published of Field.t
  | RemovedFromWaitingList
  | Rescheduled of Field.t
  | RoleAssigned
  | RoleUnassigned
  | SentList of Field.t
  | SettingsUpdated
  | SmtpConfigurationAdded
  | SmtpDetailsUpdated
  | SmtpPasswordUpdated
  | Stopped of Field.t
  | TenantUpdateDatabase
  | TenantUpdateDetails
  | Updated of Field.t
  | VerificationMessageResent
[@@deriving eq, show, yojson, variants, sexp_of]

type info = Info of string [@@deriving eq, show, yojson, variants, sexp_of]

type t =
  | Message of string
  | PageNotFoundMessage
[@@deriving eq, show, yojson, variants, sexp_of]

let field_message prefix field suffix =
  Format.asprintf "%s %s %s" prefix field suffix
  |> CCString.trim
  |> CCString.capitalize_ascii
;;

let handle_sihl_login_error = function
  | `Incorrect_password | `Does_not_exist -> Invalid Field.Password
;;

type control =
  | Accept of Field.t option
  | Add of Field.t option
  | AddToWaitingList
  | Apply
  | Ascending
  | Assign of Field.t option
  | Back
  | Cancel of Field.t option
  | Choose of Field.t option
  | Close of Field.t option
  | Create of Field.t option
  | Decline
  | Delete of Field.t option
  | Descending
  | Disable
  | Duplicate of Field.t option
  | Edit of Field.t option
  | Enable
  | Enroll
  | EnterNewCellPhone
  | Filter of Field.t option
  | Login
  | Manage of Field.t
  | MarkAsDeleted
  | More
  | NextPage
  | PleaseSelect
  | PreviousPage
  | Publish of Field.t option
  | Register
  | RemoveFromWaitingList
  | Reschedule of Field.t option
  | Resend of Field.t option
  | Reset
  | ResetForm
  | ResetPlainText
  | Save of Field.t option
  | SelectAll of Field.t option
  | SelectFilePlaceholder
  | Send of Field.t option
  | SendResetLink
  | Show
  | SignUp
  | Stop of Field.t option
  | ToggleAll
  | Unassign of Field.t option
  | Update of Field.t option
  | UpdateOrder
  | Verify of Field.t option
[@@deriving eq, show, yojson, variants, sexp_of]

let to_conformist_error error_list =
  CCList.map (fun (name, _, msg) -> name |> Field.read, msg) error_list
  |> conformist
;;

let add_field_query_params url params =
  let open CCList in
  let open Uri in
  map (CCPair.map_fst Field.show) params
  |> add_query_params' (of_string url)
  |> fun uri ->
  with_query uri (query uri |> rev |> uniq ~eq:Utils.equal_key |> rev)
  |> to_string
;;

module Collection = struct
  type t =
    { error : error list
    ; warning : warning list
    ; success : success list
    ; info : info list
    }
  [@@deriving eq, show, yojson, sexp_of]

  let empty = { error = []; warning = []; success = []; info = [] }
  let set_success txts message = { message with success = txts }
  let set_warning txts message = { message with warning = txts }
  let set_error txts message = { message with error = txts }
  let set_info txts message = { message with info = txts }

  let of_string str =
    let json =
      try Some (Yojson.Safe.from_string str) with
      | _ -> None
    in
    match json with
    | Some json -> Some (t_of_yojson json)
    | None -> None
  ;;

  let to_string t = yojson_of_t t |> Yojson.Safe.to_string
end
