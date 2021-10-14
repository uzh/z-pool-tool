(* Testable *)
let event = Alcotest.testable Pool_event.pp Pool_event.equal

let tenant_smtp_auth =
  Alcotest.testable Tenant.SmtpAuth.pp Tenant.SmtpAuth.equal
;;
