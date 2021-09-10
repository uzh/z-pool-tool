(* All events that are possible in the whole system *)
type t =
  | Participant of Participant.event
  | Admin of Admin.event
  | EmailAddress of Common_user.Event.Email.event
  | Tenant of Tenant.event
[@@deriving eq, show]

let participant events = Participant events
let admin events = Admin events
let tenant events = Tenant events
let email_address events = EmailAddress events
