(* Testable *)
let event = Alcotest.testable Pool_event.pp Pool_event.equal
let error = Alcotest.testable Pool_common.Error.pp Pool_common.Error.equal
