(* All events that are possible in the whole system *)
type event =
  | ParticipantEvents of Participant.event
  | TenantEvents of Tenant.event
[@@deriving eq, show]

let participant_events events = ParticipantEvents events
let tenant_events events = TenantEvents events

(* Testable *)
let event = Alcotest.testable pp_event equal_event
