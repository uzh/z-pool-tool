module Participant_command = Cqrs_command.Participant_command

let sign_up_not_allowed_suffix () =
  let allowed_email_suffixes = [ "@gmail.com" ] in
  let command =
    Participant_command.SignUp.
      { email = "john@bluewin.ch"
      ; password = "password"
      ; firstname = "Jane"
      ; lastname = "Doe"
      ; recruitment_channel = Participant.RecruitmentChannel.Friend
      }
  in
  let events =
    Participant_command.SignUp.handle command ~allowed_email_suffixes
    |> Result.map (List.map Utils.participant_events)
  in
  let expected = Error "Invalid email suffix provided" in
  Alcotest.(check (result (list Utils.event) string) "succeeds" expected events)
;;

let sign_up () =
  let allowed_email_suffixes = [ "@gmail.com" ] in
  let command =
    Participant_command.SignUp.
      { email = "john@gmail.com"
      ; password = "password"
      ; firstname = "Jane"
      ; lastname = "Doe"
      ; recruitment_channel = Participant.RecruitmentChannel.Friend
      }
  in
  let events =
    Participant_command.SignUp.handle command ~allowed_email_suffixes
    |> Result.map (List.map Utils.participant_events)
  in
  let expected = Ok [] in
  Alcotest.(check (result (list Utils.event) string) "succeeds" expected events)
;;
