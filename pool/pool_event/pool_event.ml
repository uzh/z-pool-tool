(* All events that are possible in the whole system *)
type t =
  | Participant of Participant.event
  | Admin of Admin.event
  | EmailAddress of Common_user.Event.Email.event
  | Tenant of Tenant.event
  | Experiment of Experiment.event
[@@deriving eq, show]

let participant events = Participant events
let admin events = Admin events
let tenant events = Tenant events
let email_address events = EmailAddress events
let experiment events = Experiment events

let handle_event = function
  | Participant event -> Participant.handle_event event
  | Admin event -> Admin.handle_event event
  | EmailAddress event -> Common_user.Event.Email.handle_event event
  | Tenant event -> Tenant.handle_event event
  | Experiment event -> Experiment.handle_event event
;;
