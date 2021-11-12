(* Testable *)
let event = Alcotest.testable Pool_event.pp Pool_event.equal

let error =
  Alcotest.testable Pool_common.Message.pp_error Pool_common.Message.equal_error
;;
