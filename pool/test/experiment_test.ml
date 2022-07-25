module ExperimentCommand = Cqrs_command.Experiment_command
module Common = Pool_common

let experiment_boolean_fields =
  Pool_common.Message.Field.(
    [ DirectRegistrationDisabled; RegistrationDisabled; AllowUninvitedSignup ]
    |> CCList.map show)
;;

module Data = struct
  let title = "New experiment"
  let public_title = "public_experiment_title"
  let description = "Description"
  let filter = "1=1"
end

let database_label = Test_utils.Data.database_label

let create () =
  let experiment = Test_utils.create_experiment () in
  let events = Ok [ Experiment.Created experiment |> Pool_event.experiment ] in
  let expected =
    Ok [ Experiment.Created experiment |> Pool_event.experiment ]
  in
  Alcotest.(
    check
      (result (list Test_utils.event) Test_utils.error)
      "succeeds"
      expected
      events)
;;

let create_without_title () =
  let events =
    let open CCResult.Infix in
    Pool_common.Message.Field.
      [ Title |> show, [ "" ]
      ; PublicTitle |> show, [ "public_title" ]
      ; Description |> show, [ Data.description ]
      ]
    |> Http_utils.format_request_boolean_values experiment_boolean_fields
    |> ExperimentCommand.Create.decode
    >>= ExperimentCommand.Create.handle
  in
  let expected =
    Error Common.Message.(Conformist [ Field.Title, Invalid Field.Title ])
  in
  Alcotest.(
    check
      (result (list Test_utils.event) Test_utils.error)
      "succeeds"
      expected
      events)
;;

let update () =
  let experiment = Test_utils.create_experiment () in
  let open CCResult.Infix in
  let events =
    Pool_common.Message.Field.
      [ Title |> show, [ Data.title ]
      ; Description |> show, [ Data.description ]
      ]
    |> ExperimentCommand.Update.decode
    >>= ExperimentCommand.Update.handle experiment
  in
  let expected =
    Pool_common.Message.Field.
      [ Title |> show, [ Data.title ]
      ; Description |> show, [ Data.description ]
      ]
    |> ExperimentCommand.Update.decode
    >>= ExperimentCommand.Update.handle experiment
  in
  Alcotest.(
    check
      (result (list Test_utils.event) Test_utils.error)
      "succeeds"
      expected
      events)
;;

let delete_with_sessions () =
  let experiment = Test_utils.create_experiment () in
  let events =
    let session_count = 1234 in
    ExperimentCommand.Delete.(
      handle { experiment_id = experiment.Experiment.id; session_count })
  in
  let expected = Error Pool_common.Message.ExperimentSessionCountNotZero in
  Alcotest.(
    check
      (result (list Test_utils.event) Test_utils.error)
      "succeeds"
      expected
      events)
;;

let urlencoded =
  [ "title", [ "The Wallet Game" ]
  ; "public_title", [ "the_wallet_game" ]
  ; "description", [ "Description." ]
  ]
;;

let create_with_missing_text_element additional error =
  let open CCResult in
  let events =
    let open CCResult.Infix in
    let open Cqrs_command.Experiment_command.Create in
    urlencoded @ additional
    |> Http_utils.format_request_boolean_values experiment_boolean_fields
    |> decode
    >>= handle
  in
  let expected = Error error in
  Alcotest.(
    check
      (result (list Test_utils.event) Test_utils.error)
      "succeeds"
      expected
      events)
;;

let with_missing_invitation_text () =
  create_with_missing_text_element
    [ "invitation_subject", [ "Invitation Subject" ] ]
    Pool_common.Message.InvitationSubjectAndTextRequired
;;

let with_missing_reminder_subject () =
  create_with_missing_text_element
    [ "reminder_text", [ "Session reminder text" ] ]
    Pool_common.Message.ReminderSubjectAndTextRequired
;;
