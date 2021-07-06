module Participant_command = Cqrs_command.Participant_command

let sign_up_invalid_channel () =
  let command =
    Participant_command.Sign_up.
      { email = "john@bluewin.ch"
      ; password = "password"
      ; firstname = "John"
      ; lastname = "Doe"
      ; recruitment_channel = "invalid channel"
      }
  in
  let events = Participant_command.Sign_up.handle command in
  let expected = Error "Invalid recruitment channel provided" in
  Alcotest.(check (result (list Utils.event) string) "succeeds" expected events)
;;

let sign_up_not_allowed_suffix () =
  let allowed_email_suffixes = [ "@gmail.com" ] in
  let command =
    Participant_command.Sign_up.
      { email = "john@bluewin.ch"
      ; password = "password"
      ; firstname = "Jane"
      ; lastname = "Doe"
      ; recruitment_channel = "friend"
      }
  in
  let events =
    Participant_command.Sign_up.handle command ~allowed_email_suffixes
  in
  let expected = Error "Invalid email suffix provided" in
  Alcotest.(check (result (list Utils.event) string) "succeeds" expected events)
;;

let sign_up () =
  let allowed_email_suffixes = [ "@gmail.com" ] in
  let command =
    Participant_command.Sign_up.
      { email = "john@gmail.com"
      ; password = "password"
      ; firstname = "Jane"
      ; lastname = "Doe"
      ; recruitment_channel = "friend"
      }
  in
  let events =
    Participant_command.Sign_up.handle command ~allowed_email_suffixes
  in
  let expected = Ok [] in
  Alcotest.(check (result (list Utils.event) string) "succeeds" expected events)
;;
