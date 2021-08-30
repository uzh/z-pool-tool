(* All events that are possible in the whole system *)
type event =
  | PersonEvents of Person.event
  | TenantEvents of Tenant.event
[@@deriving eq, show]

let person_events events = PersonEvents events
let tenant_events events = TenantEvents events

(* Testable *)
let event = Alcotest.testable pp_event equal_event
