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
        [ "token", [ token ]
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

let suite =
  Alcotest_lwt.
    [ "password reset", [ test_case "password reset" `Quick reset_password_succeeds ] ]
;;
