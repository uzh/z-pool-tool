module Participant_command = Cqrs_command.Participant_command

let sign_up_not_allowed_suffix _switch () =
  let%lwt events =
    let open Lwt_result.Syntax in
    let* allowed_email_suffixes =
      [ "gmail.com" ]
      |> CCList.map (fun suffix -> Settings.EmailSuffix.create suffix)
      |> CCResult.flatten_l
      |> Lwt.return
    in
    let command =
      CCResult.get_exn
      @@ Participant_command.SignUp.decode
           Pool_common.Message.Field.
             [ Email |> show, [ "john@bluewin.com" ]
             ; Password |> show, [ "password" ]
             ; Firstname |> show, [ "Jane" ]
             ; Lastname |> show, [ "Doe" ]
             ; ( RecruitmentChannel |> show
               , [ Participant.RecruitmentChannel.Friend
                   |> Participant.RecruitmentChannel.to_string
                 ] )
             ]
    in
    Participant_command.SignUp.handle
      ~actor:Guard.console_authorizable
      None
      command
      ~allowed_email_suffixes
  in
  let expected = Error Pool_common.Message.(Invalid Field.EmailSuffix) in
  Alcotest.(
    check
      (result (list Test_utils.event) Test_utils.error)
      "succeeds"
      expected
      events)
  |> Lwt.return
;;

let sign_up () =
  let%lwt events =
    let open Lwt_result.Syntax in
    let* allowed_email_suffixes =
      [ "gmail.com" ]
      |> CCList.map (fun suffix -> Settings.EmailSuffix.create suffix)
      |> CCResult.flatten_l
      |> Lwt.return
    in
    let command =
      CCResult.get_exn
      @@ Participant_command.SignUp.decode
           Pool_common.Message.Field.
             [ Email |> show, [ "john@gmail.com" ]
             ; Password |> show, [ "password" ]
             ; Firstname |> show, [ "Jane" ]
             ; Lastname |> show, [ "Doe" ]
             ; ( RecruitmentChannel |> show
               , [ Participant.RecruitmentChannel.Friend
                   |> Participant.RecruitmentChannel.to_string
                 ] )
             ]
    in
    Participant_command.SignUp.handle
      ~actor:Guard.console_authorizable
      (Some Pool_common.Language.En)
      command
      ~allowed_email_suffixes
  in
  let expected = Ok [] in
  Lwt.return
    Alcotest.(
      check
        (result (list Test_utils.event) Test_utils.error)
        "succeeds"
        expected
        events)
;;
