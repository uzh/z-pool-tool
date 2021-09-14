module Participant_command = Cqrs_command.Participant_command

let sign_up_not_allowed_suffix () =
  let allowed_email_suffixes = [ "@gmail.com" ] in
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
  let events =
    Participant_command.SignUp.handle command ~allowed_email_suffixes
  in
  let expected = Error "Invalid email suffix provided" in
  Alcotest.(check (result (list Utils.event) string) "succeeds" expected events)
;;

let sign_up () =
  let allowed_email_suffixes = [ "@gmail.com" ] in
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
  let events =
    Participant_command.SignUp.handle command ~allowed_email_suffixes
  in
  let expected = Ok [] in
  Alcotest.(check (result (list Utils.event) string) "succeeds" expected events)
;;
