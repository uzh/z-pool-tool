open Alcotest_lwt
module PasswordReset = Password_reset_test

let testable_password = Pool_user.Password.(Alcotest.testable pp equal)
let testable_email = Pool_user.EmailAddress.(Alcotest.testable pp equal)
let alcotest = Pool_user.(Alcotest.testable pp equal)
let database_label = Test_utils.Data.database_label

let created_password =
  Pool_user.Password.create "CD&*BA8txf3mRuGF" |> Test_utils.get_or_failwith
;;

let validate_valid_password =
  Test_utils.case
  @@ fun () ->
  let password = Pool_user.Password.create "CD&*BA8txf3mRuGF" in
  Alcotest.(
    check
      (result testable_password Test_utils.error)
      "valid password"
      (Ok created_password)
      password);
  let password = password |> Test_utils.get_or_failwith in
  let actual =
    Pool_user.Password.validate_password_confirmation
      password
      (password |> Pool_user.Password.to_confirmed)
  in
  Alcotest.(check (result unit Test_utils.error) "is valid" (Ok ()) actual);
  Lwt.return_ok ()
;;

let validate_invalid_password =
  Test_utils.case
  @@ fun () ->
  let invalid_passwords =
    Pool_message.Error.
      [ "123", PasswordPolicyMinLength 8
      ; "12345678", PasswordPolicyCapitalLetter
      ; "Abcdefgh", PasswordPolicyNumber
      ; ( "A1234567"
        , PasswordPolicySpecialChar
            Pool_user.Password.Policy.default_special_char_set )
      ]
  in
  let check_invalid (plain, error) =
    let password = Pool_user.Password.create plain in
    Alcotest.(
      check
        (result testable_password Test_utils.error)
        [%string "invalid password: %{plain}"]
        (Error error)
        password);
    ()
  in
  let () = CCList.iter check_invalid invalid_passwords in
  Lwt.return_ok ()
;;

let json_serialization =
  Test_utils.case
  @@ fun () ->
  let%lwt user =
    let open Pool_user in
    create_user
      database_label
      (EmailAddress.of_string "foobar@example.com")
      (Lastname.of_string "Doe")
      (Firstname.of_string "Jane")
      created_password
  in
  let user_after = user |> Pool_user.yojson_of_t |> Pool_user.t_of_yojson in
  let user = Format.asprintf "%a" Pool_user.pp user in
  let user_after = Format.asprintf "%a" Pool_user.pp user_after in
  Alcotest.(check string "is same user" user_after user);
  Lwt.return_ok ()
;;

let update_details =
  Test_utils.case
  @@ fun () ->
  let open Pool_user in
  let%lwt user =
    create_user
      database_label
      (EmailAddress.of_string "foobar2@example.com")
      (Lastname.of_string "Star")
      (Firstname.of_string "Jane")
      created_password
  in
  let%lwt updated_user =
    update database_label user ~email:(EmailAddress.of_string "new@example.com")
  in
  let actual_email = updated_user.Pool_user.email |> EmailAddress.value in
  let actual_name = updated_user.Pool_user.name |> Lastname.value in
  Alcotest.(check string "Has updated email" "new@example.com" actual_email);
  Alcotest.(check string "Hasn't updated name" "Star" actual_name);
  Lwt.return_ok ()
;;

let update_password =
  Test_utils.case
  @@ fun () ->
  let open Pool_user in
  let email_address = EmailAddress.of_string "foobar3@example.com" in
  let new_password =
    Password.create "Password1!" |> Test_utils.get_or_failwith
  in
  let%lwt user =
    create_user
      database_label
      email_address
      (Lastname.of_string "Star")
      (Firstname.of_string "Jane")
      created_password
  in
  let%lwt _ =
    Pool_user.update_password
      database_label
      user
      ~old_password:created_password
      ~new_password
      ~new_password_confirmation:(new_password |> Password.to_confirmed)
    |> Lwt.map Result.get_ok
  in
  let%lwt user =
    Pool_user.login database_label email_address new_password
    |> Lwt.map Result.get_ok
  in
  let actual_email = user.Pool_user.email in
  Alcotest.(
    check
      testable_email
      "Can login with updated password"
      email_address
      actual_email);
  Lwt.return_ok ()
;;

let update_password_fails =
  Test_utils.case
  @@ fun () ->
  let open Pool_user in
  let email_address = EmailAddress.of_string "foobar4@example.com" in
  let new_password =
    Password.create "Password1!" |> Test_utils.get_or_failwith
  in
  let%lwt user =
    create_user
      database_label
      email_address
      (Lastname.of_string "Star")
      (Firstname.of_string "Jane")
      created_password
  in
  let%lwt change_result =
    update_password
      database_label
      user
      ~old_password:
        (Password.create_unvalidated "wrong_old_password"
         |> Test_utils.get_or_failwith)
      ~new_password
      ~new_password_confirmation:(new_password |> Password.to_confirmed)
  in
  Alcotest.(
    check
      (result alcotest Test_utils.error)
      "Can login with updated password"
      (Error Pool_message.(Error.Invalid Field.CurrentPassword))
      change_result);
  Lwt.return_ok ()
;;

let find_by_email_is_case_insensitive =
  Test_utils.case
  @@ fun () ->
  let open Pool_user in
  let email_addresses =
    [ "user1@example.com"; "user2@example.com" ]
    |> CCList.map EmailAddress.of_string
  in
  let%lwt (_ : t list) =
    Lwt_list.map_s
      (fun email ->
        create_user
          database_label
          email
          (Lastname.of_string "Star")
          (Firstname.of_string "Jane")
          created_password)
      email_addresses
  in
  let%lwt user =
    EmailAddress.of_string "User1@Example.com"
    |> Pool_user.find_by_email_opt database_label
  in
  match user with
  | Some _ -> Lwt.return_ok ()
  | None -> Alcotest.fail "expected to find user with email"
;;

let filter_users_by_email_returns_single_user =
  Test_utils.case
  @@ fun () ->
  let open Pool_user in
  let email_addresses =
    [ "foo@example.com"; "fooz@example.com"; "bar@example.com" ]
    |> CCList.map EmailAddress.of_string
  in
  let%lwt (users : t list) =
    Lwt_list.map_s
      (fun email ->
        create_user
          database_label
          email
          (Lastname.of_string "Star")
          (Firstname.of_string "Jane")
          created_password)
      email_addresses
  in
  let%lwt actual_user =
    EmailAddress.of_string "fooz@example.com"
    |> Pool_user.find_by_email database_label
  in
  Alcotest.(
    check
      alcotest
      "has one user"
      (CCList.last 2 users |> CCList.head_opt |> CCOption.get_exn_or "failed")
      actual_user);
  Lwt.return_ok ()
;;

module Web = struct
  let fake_token = "faketoken"

  let read_token (user_id : Pool_user.Id.t) (_ : Database.Label.t) token ~k =
    if String.equal k "user_id" && String.equal token fake_token
    then Lwt.return_some user_id
    else Lwt.return_none
  ;;

  let user_from_token =
    Test_utils.case
    @@ fun () ->
    let open Pool_user in
    let email_address = EmailAddress.of_string "foo2@example.com" in
    let%lwt user =
      create_user
        database_label
        email_address
        (Lastname.of_string "Star")
        (Firstname.of_string "Jane")
        created_password
    in
    let read_token = read_token user.Pool_user.id in
    let token_header = Format.sprintf "Bearer %s" fake_token in
    let req =
      Opium.Request.get "/some/path/login"
      |> Opium.Request.add_header ("authorization", token_header)
    in
    let handler req =
      let%lwt user =
        Pool_user.Web.user_from_token database_label read_token req
      in
      let email = CCOption.map (fun user -> user.Pool_user.email) user in
      Alcotest.(
        check
          (option testable_email)
          "has same email"
          (Some (EmailAddress.of_string "foo2@example.com"))
          email);
      Lwt.return @@ Opium.Response.of_plain_text ""
    in
    let%lwt _ = handler req in
    Lwt.return_ok ()
  ;;

  let user_from_session =
    Test_utils.case
    @@ fun () ->
    let open Pool_user in
    let email_address = EmailAddress.of_string "foo3@example.com" in
    let%lwt user =
      create_user
        database_label
        email_address
        (Lastname.of_string "Star")
        (Firstname.of_string "Jane")
        created_password
    in
    let cookie =
      Sihl.Web.Response.of_plain_text ""
      |> Sihl.Web.Session.set
           [ "user_id", user.Pool_user.id |> Pool_user.Id.value ]
      |> Sihl.Web.Response.cookie "_session"
      |> Option.get
    in
    let req =
      Opium.Request.get "/some/path/login"
      |> Opium.Request.add_cookie cookie.Sihl.Web.Cookie.value
    in
    let handler req =
      let%lwt user = Pool_user.Web.user_from_session database_label req in
      let email = Option.map (fun user -> user.Pool_user.email) user in
      Alcotest.(
        check
          (option testable_email)
          "has same email"
          (Some (EmailAddress.of_string "foo3@example.com"))
          email);
      Lwt.return @@ Opium.Response.of_plain_text ""
    in
    let%lwt _ = handler req in
    Lwt.return_ok ()
  ;;
end

let suite =
  [ ( "user service"
    , [ test_case "validate valid password" `Quick validate_valid_password
      ; test_case "validate invalid password" `Quick validate_invalid_password
      ; test_case "json serialization" `Quick json_serialization
      ; test_case "update details" `Quick update_details
      ; test_case "update password" `Quick update_password
      ; test_case "update password fails" `Quick update_password_fails
      ; test_case
          "find by email is case insensitive"
          `Quick
          find_by_email_is_case_insensitive
      ; test_case
          "filter users by email returns single user"
          `Quick
          filter_users_by_email_returns_single_user
      ] )
  ; ( "user service web"
    , [ test_case "user from token" `Quick Web.user_from_token
      ; test_case "user from session" `Quick Web.user_from_session
      ] )
  ]
  @ Password_reset_test.suite
;;

let () =
  let services = [ Pool_database.register (); Pool_token.register () ] in
  Lwt_main.run
    (let%lwt () = Test_utils.setup_test () in
     let%lwt _ = Sihl.Container.start_services services in
     let%lwt () = Database.clean_all database_label in
     Alcotest_lwt.run "pool_user_test" suite)
;;
