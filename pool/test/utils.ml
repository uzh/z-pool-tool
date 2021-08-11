(* All events that are possible in the whole system *)
type event = Participant.event
(* | ParticipantEvents of Participant.event *)
(* | TenantEvents of Tenant.event *)
[@@deriving eq, show]

let event = Alcotest.testable pp_event equal_event
