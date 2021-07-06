module Participant_command = Cqrs_command.Participant_command

let sign_up_invalid_channel () =
  let allowed_email_suffixes = [ "@gmail.com" ] in
  let command =
    Participant_command.Sign_up.
      { email = "walter@bluewin.ch"
      ; password = "password"
      ; firstname = "Walter"
      ; lastname = "White"
      ; recruitment_channel = "friends"
      ; terms_accepted_at = Sihl.now ()
      }
  in
  let events =
    Participant_command.Sign_up.handle command ~allowed_email_suffixes
  in
  let expected = Ok [] in
  Alcotest.(check (result (list Utils.event) string) "succeeds" expected events)
;;
