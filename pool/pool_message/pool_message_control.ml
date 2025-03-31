open Sexplib.Conv
open Ppx_yojson_conv_lib.Yojson_conv

type t =
  | Accept of Field.t option
  | Add of Field.t option
  | AddToWaitingList
  | AllSessions of int
  | AllAvailableExperiments of int
  | Apply
  | Ascending
  | Assign of Field.t option
  | Back
  | Cancel of Field.t option
  | ChangeSession
  | Choose of Field.t option
  | Close of Field.t option
  | Create of Field.t option
  | Decline
  | Delete of Field.t option
  | Descending
  | Details
  | Disable
  | Duplicate of Field.t option
  | Edit of Field.t option
  | Enable
  | Enroll
  | EnterNewCellPhone
  | Filter of Field.t option
  | Generate
  | Hide of Field.t option
  | Ignore of Field.t option
  | LoadDefaultTemplate
  | Login
  | Manage of Field.t
  | MarkAsDeleted
  | More
  | NextPage
  | OpenProfile
  | PauseAccount
  | PleaseSelect
  | PreviousPage
  | Print of Field.t option
  | PromoteContact
  | PublicPage
  | Publish of Field.t option
  | ReactivateAccount
  | Register
  | Remove of Field.t option
  | RemoveFromWaitingList
  | Reschedule of Field.t option
  | Resend of Field.t option
  | Reset of Field.t option
  | ResetForm
  | ResetPlainText
  | Resume of Field.t option
  | Save of Field.t option
  | Select
  | SelectAll of Field.t option
  | SelectFilePlaceholder
  | Send of Field.t option
  | SendResetLink
  | SessionDetails
  | Show
  | SignUp
  | Start of Field.t option
  | Stop of Field.t option
  | ToggleAll
  | Unassign of Field.t option
  | Unverify
  | Update of Field.t option
  | UpdateAssignmentsMatchFilter
  | UpdateOrder
  | Validate
  | Verify of Field.t option
[@@deriving eq, show, yojson, sexp_of]
(* NOTE: removed variants because "Show" overwrites "show" function *)

let delete m = Delete m
let stop m = Stop m
