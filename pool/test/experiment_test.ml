module ExperimentCommand = Cqrs_command.Experiment_command
module Common = Pool_common

let database_label = Test_utils.Data.database_label

let create () =
  let events =
    let open CCResult.Infix in
    Pool_common.Message.Field.
      [ Title |> show, [ "New Experiment" ]
      ; Description |> show, [ "Some description" ]
      ]
    |> ExperimentCommand.Create.decode
    >>= ExperimentCommand.Create.handle
  in
  let expected =
    let open CCResult in
    let open Experiment in
    let* title = Title.create "New Experiment" in
    let* description = Description.create "Some description" in
    let create = { title; description } in
    Ok [ Experiment.Created create |> Pool_event.experiment ]
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
      [ Title |> show, [ "" ]; Description |> show, [ "Some description" ] ]
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
