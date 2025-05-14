open Test_utils
open Utils.Lwt_result.Infix
module Request = Test_request

let pool = Data.database_label

module Data = struct
  let contact_id = Contact.Id.create ()
  let auth_id = Authentication.Id.create ()
  let auth_token = Authentication.Token.generate ()
  let password = Model.password
  let password_plain = Model.password_str
end

module Testable = struct
  type create_2fa = Pool_user.t * Authentication.t * Pool_event.t list
  [@@deriving show, eq]

  let testable_create = Alcotest.(testable pp_create_2fa equal_create_2fa)

  type confirm_2fa = Pool_user.t * Pool_event.t list [@@deriving show, eq]

  let testable_confirm = Alcotest.(testable pp_confirm_2fa equal_confirm_2fa)

  let check test msg expected generated =
    Alcotest.(check (result test Test_utils.error) msg expected generated)
  ;;
end

let setup_test ?contact_id () =
  let open Integration_utils in
  let%lwt user = ContactRepo.create ?id:contact_id ~password:Data.password () in
  Lwt.return user
;;

let setup_request urlencoded =
  let%lwt tenant = Pool_tenant.find_by_label pool ||> get_or_failwith in
  Request.mock_post_request ~context_tenant:tenant urlencoded |> Lwt.return
;;

let handle_post_create ?auth data =
  let open Handler.Helpers.Login in
  let open CCOption in
  let urlencoded = Request.data_to_urlencoded data in
  let%lwt req = setup_request urlencoded in
  let context = Pool_context.find_exn req in
  create_2fa_login ?id:(map fst auth) ?token:(map snd auth) req context urlencoded
;;

let check_post_create ?auth message data expected =
  let%lwt res = handle_post_create ?auth data in
  Testable.check Testable.testable_create message expected res |> Lwt.return
;;

let check_post_confirmation message data expected =
  let open Handler.Helpers.Login in
  let urlencoded = Request.data_to_urlencoded data in
  let%lwt req = setup_request urlencoded in
  let tags = Pool_context.Logger.Tags.req req in
  let%lwt res =
    let* user, auth, token = decode_2fa_confirmation pool req ~tags in
    confirm_2fa_login ~tags user auth token req
  in
  Testable.check Testable.testable_confirm message expected res |> Lwt.return
;;

let login_too_many_attempts_test _ () =
  let open Pool_message in
  let%lwt user = setup_test () in
  let email = user |> Contact.email_address |> Pool_user.EmailAddress.value in
  let data = [ "email", email; "password", "wrong-pw" ] in
  let rec attempt_login n =
    let error = Error Error.LoginInvalidEmailPassword in
    if n = 0
    then Lwt.return ()
    else (
      let%lwt () = check_post_create "invalid password" data error in
      attempt_login (n - 1))
  in
  let%lwt () = attempt_login 4 in
  let%lwt res =
    handle_post_create data
    ||> function[@warning "-4"]
    | Error (Error.AccountTemporarilySuspended _) -> true
    | _ -> false
  in
  Alcotest.(check bool) "Account temporarily suspended" true res;
  Lwt.return_unit
;;

let failed_login_test _ () =
  let open Pool_message in
  let%lwt user = setup_test () in
  let email = user |> Contact.email_address |> Pool_user.EmailAddress.value in
  (* Missing email or password *)
  let error = Error Error.LoginProvideDetails in
  let%lwt () = check_post_create "Without password" [ "email", email ] error in
  let%lwt () = check_post_create "Without email" [ "password", "any_password" ] error in
  let login_err = Error Error.LoginInvalidEmailPassword in
  (* Login with inexisting email *)
  let data = [ "email", "not-existing@email.com"; "password", "wrong_password" ] in
  let%lwt () = check_post_create "not existing email" data login_err in
  (* Login with invalid password *)
  let data = [ "email", email; "password", "wrong_password" ] in
  check_post_create "invalid password" data login_err
;;

let successful_create_2fa_test _ () =
  let open Authentication in
  let open Data in
  let%lwt contact = setup_test ~contact_id () in
  let email = contact |> Contact.email_address |> Pool_user.EmailAddress.value in
  let data = [ "email", email; "password", password_plain ] in
  let%lwt expected =
    let user = Contact.user contact in
    let auth = (create ~id:auth_id ~token:auth_token ~user ~channel:Channel.Email) () in
    let%lwt tenant = Pool_tenant.find_by_label pool ||> get_or_failwith in
    let%lwt email =
      let open Message_template in
      Login2FAToken.prepare pool Pool_common.Language.En (Tenant tenant)
      ||> fun make_make -> make_make user auth
    in
    let events =
      [ Created auth |> Pool_event.authentication; Email.sent email |> Pool_event.email ]
    in
    Lwt.return (user, auth, events)
  in
  let _, _, events = expected in
  let%lwt () = Pool_event.handle_events pool (Pool_context.contact contact) events in
  check_post_create ~auth:(auth_id, auth_token) "successful login" data (Ok expected)
;;

let confirm_2fa_test _ () =
  let open Data in
  let open Authentication in
  let open Pool_message in
  let check = check_post_confirmation in
  let token_value = Token.value auth_token in
  let id_value = Id.value auth_id in
  let%lwt contact = Contact.find pool Data.contact_id ||> get_or_failwith in
  (* Missing token or auth_id *)
  let no_value field = Error Error.(Conformist [ field, NoValue ]) in
  let%lwt () = check "Without token" [ "id", id_value ] (no_value Field.Token) in
  let%lwt () = check "Without id" [ "token", token_value ] (no_value Field.Id) in
  (* Invalid auth id *)
  let invalid_token = Error Error.(Invalid Field.Token) in
  let%lwt () =
    let data = [ ("id", Id.(create () |> value)); "token", token_value ] in
    check "Invalid id" data invalid_token
  in
  (* Invalid token *)
  let%lwt () =
    let data = [ ("id", Id.(create () |> value)); "token", "12341234" ] in
    check "Invalid token" data invalid_token
  in
  (* Successfull login *)
  let%lwt auth, user = find_valid_by_id pool auth_id ||> get_or_failwith in
  let events = [ Deleted auth |> Pool_event.authentication ] in
  let%lwt () =
    let data = [ "id", id_value; "token", token_value ] in
    check "successfull 2fa logjn" data (Ok (user, events))
  in
  let%lwt () = Pool_event.handle_events pool (Pool_context.contact contact) events in
  Lwt.return_unit
;;
