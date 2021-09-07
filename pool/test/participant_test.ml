module Participant_command = Cqrs_command.Participant_command

let sign_up_not_allowed_suffix () =
  let allowed_email_suffixes = [ "@gmail.com" ] in
  let command =
    Participant_command.Sign_up.
      { email = "john@bluewin.ch"
      ; token = "email_token"
      ; password = "password"
      ; firstname = "Jane"
      ; lastname = "Doe"
      ; recruitment_channel = Participant.RecruitmentChannel.Friend
      }
  in
  let events =
    Participant_command.Sign_up.handle command ~allowed_email_suffixes
    |> Result.map (List.map Utils.participant_events)
  in
  let expected = Error "Invalid email suffix provided" in
  Alcotest.(check (result (list Utils.event) string) "succeeds" expected events)
;;

let sign_up () =
  let allowed_email_suffixes = [ "@gmail.com" ] in
  let command =
    Participant_command.Sign_up.
      { email = "john@gmail.com"
      ; token = "email_token"
      ; password = "password"
      ; firstname = "Jane"
      ; lastname = "Doe"
      ; recruitment_channel = Participant.RecruitmentChannel.Friend
      }
  in
  let events =
    Participant_command.Sign_up.handle command ~allowed_email_suffixes
    |> Result.map (List.map Utils.participant_events)
  in
  let expected = Ok [] in
  Alcotest.(check (result (list Utils.event) string) "succeeds" expected events)
;;
