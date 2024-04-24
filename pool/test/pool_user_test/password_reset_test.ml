let database_label = Test_utils.Data.database_label

let created_password =
  Pool_user.Password.create "CD&*BA8txf3mRuGF" |> Test_utils.get_or_failwith
;;

let reset_password_suceeds =
  Test_utils.case (fun () ->
    let open Pool_user in
    let open Utils.Lwt_result.Infix in
    let email = EmailAddress.of_string "foopass@example.com" in
    let new_password =
      Password.create "Password1!" |> Test_utils.get_or_failwith
    in
    let%lwt (_ : t) =
      create_user
        database_label
        email
        (Lastname.of_string "Star")
        (Firstname.of_string "Jane")
        created_password
    in
    let%lwt token =
      Pool_user.PasswordReset.create_reset_token database_label email
      ||> CCOption.get_exn_or "User with email not found"
    in
    let%lwt () =
      Pool_user.PasswordReset.reset_password
        database_label
        ~token
        new_password
        (new_password |> Password.to_confirmed)
      ||> Test_utils.get_or_failwith
    in
    let%lwt (_ : t) =
      login database_label email new_password ||> CCResult.get_exn
    in
    Lwt.return_ok ())
;;

let suite =
  Alcotest_lwt.
    [ ( "password reset"
      , [ test_case "password reset" `Quick reset_password_suceeds ] )
    ]
;;
