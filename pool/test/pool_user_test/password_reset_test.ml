let database_label = Test_utils.Data.database_label
let created_password = Pool_user.Password.Plain.create "CD&*BA8txf3mRuGF"
let created_password_confirmation = Pool_user.Password.to_confirmed created_password

let reset_password_succeeds =
  Test_utils.case (fun () ->
    let open Pool_user in
    let open Utils.Lwt_result.Infix in
    let email = EmailAddress.of_string "foopass@example.com" in
    let new_password = "Password1!" in
    let%lwt (_ : t) =
      create_user
        database_label
        email
        (Lastname.of_string "Star")
        (Firstname.of_string "Jane")
        created_password
        created_password_confirmation
      ||> Pool_common.Utils.get_or_failwith
    in
    let%lwt token =
      Pool_user.Password.Reset.create_token database_label email
      ||> CCOption.get_exn_or "User with email not found"
    in
    let* events =
      let open Cqrs_command.User_command.ResetPassword in
      let command_data =
        [ "token", [ Pool_token.value token ]
        ; "password", [ new_password ]
        ; "password_confirmation", [ new_password ]
        ]
      in
      command_data |> decode |> Lwt_result.lift >== handle
    in
    let%lwt () = Pool_event.handle_events database_label Pool_context.Guest events in
    let validated_password = Pool_user.Password.Plain.create new_password in
    let%lwt (_ : t) =
      login database_label email validated_password ||> CCResult.get_exn
    in
    Lwt.return_ok ())
;;

let reset_password_fail_password_policy =
  Test_utils.case (fun () ->
    let open Pool_user in
    let open Utils.Lwt_result.Infix in
    let email =
      EmailAddress.of_string "reset_password_fail_password_policy@example.com"
    in
    let new_password = "password1!" in
    let%lwt (_ : t) =
      create_user
        database_label
        email
        (Lastname.of_string "Star")
        (Firstname.of_string "Jane")
        created_password
        created_password_confirmation
      ||> Pool_common.Utils.get_or_failwith
    in
    let%lwt token =
      Pool_user.Password.Reset.create_token database_label email
      ||> CCOption.get_exn_or "User with email not found"
    in
    let%lwt result =
      let open Cqrs_command.User_command.ResetPassword in
      let command_data =
        [ "token", [ Pool_token.value token ]
        ; "password", [ new_password ]
        ; "password_confirmation", [ new_password ]
        ]
      in
      command_data |> decode |> Lwt_result.lift >== handle
    in
    Alcotest.(check (result (list Test_utils.event) Test_utils.error))
      "Expected validation error not found"
      (Error
         Pool_message.(
           Error.conformist [ Field.Password, Error.PasswordPolicyCapitalLetter ]))
      result;
    Lwt.return_ok ())
;;

let reset_password_events =
  Test_utils.case (fun () ->
    let open Pool_user in
    let open Utils.Lwt_result.Infix in
    let email = EmailAddress.of_string "reset_password_with_notification@example.com" in
    let new_password = "Password1!" in
    let%lwt _ =
      create_user
        database_label
        email
        (Lastname.of_string "Star")
        (Firstname.of_string "Jane")
        created_password
        created_password_confirmation
      ||> Pool_common.Utils.get_or_failwith
    in
    let%lwt token =
      Pool_user.Password.Reset.create_token database_label email
      ||> CCOption.get_exn_or "User with email not found"
    in
    let* events =
      let open Cqrs_command.User_command.ResetPassword in
      let command_data =
        [ "token", [ Pool_token.value token ]
        ; "password", [ new_password ]
        ; "password_confirmation", [ new_password ]
        ]
      in
      command_data |> decode |> Lwt_result.lift >== handle
    in
    let expected_events =
      [ Pool_user.PasswordReset
          ( token
          , Password.Plain.create new_password
          , Password.Confirmation.create new_password )
        |> Pool_event.user
      ]
    in
    Alcotest.(check (list Test_utils.event))
      "Expected events do not match"
      expected_events
      events;
    Lwt.return_ok ())
;;

let suite =
  Alcotest_lwt.
    [ ( "password reset"
      , [ test_case "password reset" `Quick reset_password_succeeds
        ; test_case
            "password reset fail password policy"
            `Quick
            reset_password_fail_password_policy
        ; test_case "password reset events" `Quick reset_password_events
        ] )
    ]
;;
