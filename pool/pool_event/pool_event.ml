(* All events that are possible in the whole system *)
type t =
  | Participant of Participant.event
  | Admin of Admin.event
  | Root of Root.event
  | EmailAddress of Email.event
  | Tenant of Tenant.event
  | Experiment of Experiment.event
  | Settings of Settings.event
[@@deriving eq, show]

let participant events = Participant events
let admin events = Admin events
let root events = Root events
let tenant events = Tenant events
let email_address events = EmailAddress events
let settings events = Settings events
let experiment events = Experiment events

let handle_event pool event =
  match event with
  | Participant event -> Participant.handle_event pool event
  | Admin event -> Admin.handle_event pool event
  | Root event -> Root.handle_event pool event
  | EmailAddress event -> Email.handle_event pool event
  | Tenant event -> Tenant.handle_event pool event
  | Experiment event -> Experiment.handle_event pool event
  | Settings event -> Settings.handle_event pool event
;;

let handle_events pool = Lwt_list.iter_s (handle_event pool)
