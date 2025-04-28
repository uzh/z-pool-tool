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

let failed_login_attempts _ () =
  let open Alcotest in
  let open Test_utils in
  let open Pool_user.FailedLoginAttempt in
  let testable = option failed_login_attempt in
  let pool = Data.database_label in
  let%lwt contact = Integration_utils.ContactRepo.create () in
  let email = Contact.email_address contact in
  let%lwt result = Repo.find_current pool email in
  check testable "not blocked" None result;
  let failed_attempt =
    let open CCOption.Infix in
    let open Ptime in
    let get_exn = CCOption.get_exn_or "Invalid time" in
    let blocked_until =
      add_span (Ptime_clock.now ()) Time.day
      >|= to_date
      >>= of_date
      |> get_exn
      |> BlockedUntil.of_ptime
    in
    create email Counter.init (Some blocked_until)
  in
  let%lwt () = Repo.insert pool failed_attempt in
  let%lwt result = Repo.find_current pool email in
  check testable "contact was blocked" (Some failed_attempt) result;
  let%lwt () =
    let open Pool_user in
    Unblocked (Contact.user contact) |> handle_event pool
  in
  let%lwt result = Repo.find_current pool email in
  check testable "contact was unblocked" None result;
  let past_attempt =
    let blocked_until =
      Ptime.sub_span (Ptime_clock.now ()) Time.day
      |> CCOption.get_exn_or "Invalid time"
      |> BlockedUntil.of_ptime
    in
    create email Counter.init (Some blocked_until)
  in
  let%lwt () = Repo.insert pool past_attempt in
  let%lwt result = Repo.find_current pool email in
  check testable "attempt is past" None result;
  Lwt.return_unit
;;
