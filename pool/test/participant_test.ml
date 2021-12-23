module Participant_command = Cqrs_command.Participant_command

let sign_up_not_allowed_suffix () =
  let events =
    let open CCResult in
    let* allowed_email_suffixes =
      [ "gmail.com" ]
      |> CCList.map (fun suffix -> Settings.EmailSuffix.create suffix)
      |> CCResult.flatten_l
    in
    let command =
      CCResult.get_exn
      @@ Participant_command.SignUp.decode
           [ "email", [ "john@bluewin.com" ]
           ; "password", [ "password" ]
           ; "firstname", [ "Jane" ]
           ; "lastname", [ "Doe" ]
           ; ( "recruitment_channel"
             , [ Participant.RecruitmentChannel.Friend
                 |> Participant.RecruitmentChannel.to_string
               ] )
           ]
    in
    Participant_command.SignUp.handle None command ~allowed_email_suffixes
  in
  let expected = Error Pool_common.Message.(Invalid EmailSuffix) in
  Alcotest.(
    check
      (result (list Test_utils.event) Test_utils.error)
      "succeeds"
      expected
      events)
;;

let sign_up () =
  let events =
    let open CCResult in
    let* allowed_email_suffixes =
      [ "gmail.com" ]
      |> CCList.map (fun suffix -> Settings.EmailSuffix.create suffix)
      |> CCResult.flatten_l
    in
    let command =
      CCResult.get_exn
      @@ Participant_command.SignUp.decode
           [ "email", [ "john@gmail.com" ]
           ; "password", [ "password" ]
           ; "firstname", [ "Jane" ]
           ; "lastname", [ "Doe" ]
           ; ( "recruitment_channel"
             , [ Participant.RecruitmentChannel.Friend
                 |> Participant.RecruitmentChannel.to_string
               ] )
           ]
    in
    Participant_command.SignUp.handle
      (Some Pool_common.Language.En)
      command
      ~allowed_email_suffixes
  in
  let expected = Ok [] in
  Alcotest.(
    check
      (result (list Test_utils.event) Test_utils.error)
      "succeeds"
      expected
      events)
;;
