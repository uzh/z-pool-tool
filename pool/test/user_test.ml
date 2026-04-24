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

(* Email verification resend tests *)

(** Resending verification via [Email.renew_token] must produce a new distinct
    token. The old token must no longer be active. *)
let resend_verification_deactivates_old_token _ () =
  let pool = Test_utils.Data.database_label in
  let email =
    Format.asprintf "resend-test+%s@econ.uzh.ch" Pool_common.Id.(create () |> value)
    |> Pool_user.EmailAddress.of_string
  in
  let%lwt token1 = Email.create_token pool email in
  let%lwt found = Email.find_active_token pool email in
  Alcotest.(check bool "initial token is active" true (CCOption.is_some found));
  let%lwt token2 = Email.renew_token pool email in
  let%lwt old_still_active = Pool_token.is_active pool token1 in
  Alcotest.(check bool "old token is no longer active" false old_still_active);
  Alcotest.(check bool "tokens differ" true (not (Pool_token.equal token1 token2)));
  let%lwt active = Email.find_active_token pool email in
  Alcotest.(
    check
      (option (Alcotest.testable Pool_token.pp Pool_token.equal))
      "new token is the active token"
      (Some token2)
      active);
  Lwt.return_unit
;;

(** Each call to [Email.Created] must replace the previous unverified email
    record for the same user so that only one pending verification exists. *)
let email_created_event_replaces_unverified_record _ () =
  let pool = Test_utils.Data.database_label in
  let current_user = Test_utils.Model.create_admin () in
  (* Create an admin user to hang the unverified email on *)
  let%lwt admin = Integration_utils.AdminRepo.create () in
  let user_id = Admin.id admin |> Admin.Id.to_user in
  let email =
    Format.asprintf "verify-test+%s@econ.uzh.ch" Pool_common.Id.(create () |> value)
    |> Pool_user.EmailAddress.of_string
  in
  let token_for email_addr =
    Pool_token.create pool [ "email", Pool_user.EmailAddress.value email_addr ]
  in
  (* First verification request *)
  let%lwt token1 = token_for email in
  let%lwt () =
    Email.Created (email, token1, user_id)
    |> Pool_event.email_verification
    |> Pool_event.handle_event pool current_user
  in
  let%lwt unverified1 =
    Email.find_unverified_by_user pool (Admin.Id.to_common (Admin.id admin))
  in
  let result1_token = unverified1 |> CCResult.map Email.token |> CCResult.get_exn in
  Alcotest.(
    check
      (Alcotest.testable Pool_token.pp Pool_token.equal)
      "first Created stores token1"
      token1
      result1_token);
  (* Second verification request – simulates a resend *)
  let%lwt token2 = token_for email in
  let%lwt () =
    Email.Created (email, token2, user_id)
    |> Pool_event.email_verification
    |> Pool_event.handle_event pool current_user
  in
  let%lwt unverified2 =
    Email.find_unverified_by_user pool (Admin.Id.to_common (Admin.id admin))
  in
  let result2_token = unverified2 |> CCResult.map Email.token |> CCResult.get_exn in
  (* Must now hold token2, not token1 *)
  Alcotest.(
    check
      (Alcotest.testable Pool_token.pp Pool_token.equal)
      "second Created replaces with token2"
      token2
      result2_token);
  Alcotest.(
    check bool "token2 differs from token1" true (not (Pool_token.equal token1 token2)));
  Lwt.return_unit
;;
