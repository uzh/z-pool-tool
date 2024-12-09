let validate_email_adress () =
  let open Pool_user in
  let check_result expected generated =
    let open Alcotest in
    let email = testable EmailAddress.pp EmailAddress.equal in
    check (result email Test_utils.error) "succeeds" expected generated
  in
  let valid_addresses =
    [ "it@econ.uzh.ch"
    ; "very.common@example.com"
    ; "very-common@example.com"
    ; "very_common@example.com"
    ; "very_123_common@example.com"
    ; "verycommon123@example.com"
    ; "_______@example.com"
    ; "email@example.name"
    ]
  in
  let invalid_addresses =
    [ "plainaddress"
    ; "email.example.com"
    ; "email..email@example.com"
    ; "verycommon;@example.com"
    ; "verycommon.@example.com"
    ; "very common;@example.com"
    ; "ke.vi.nkee.ne.r.le.g.a.l888.@gmail.com"
    ; "p;@hotmail.com"
    ]
  in
  let () =
    CCList.iter
      (fun email ->
         let expected = Ok (EmailAddress.of_string email) in
         let result = EmailAddress.create email in
         check_result expected result)
      valid_addresses
  in
  CCList.iter
    (fun email ->
       let expected = Error Pool_message.(Error.Invalid Field.EmailAddress) in
       let result = EmailAddress.create email in
       check_result expected result)
    invalid_addresses
;;
