module ExperimentCommand = Cqrs_command.Experiment_command
module Common = Pool_common

let experiment_boolean_fields =
  Pool_common.Message.Field.
    [ WaitingListDisabled |> show
    ; DirectRegistrationDisabled |> show
    ; RegistrationDisabled |> show
    ]
;;

module Data = struct
  let title = "New experiment"
  let description = "Description"
  let filter = "1=1"

  let experiment =
    let open CCResult in
    let id = Pool_common.Id.create () in
    let* title = title |> Experiment.Title.create in
    let* description = description |> Experiment.Description.create in
    Ok
      Experiment.
        { id
        ; title
        ; description
        ; filter
        ; mailings = []
        ; waiting_list_disabled = true |> WaitingListDisabled.create
        ; direct_registration_disabled =
            false |> DirectRegistrationDisabled.create
        ; registration_disabled = false |> RegistrationDisabled.create
        ; created_at = Common.CreatedAt.create ()
        ; updated_at = Common.UpdatedAt.create ()
        }
  ;;
end

let database_label = Test_utils.Data.database_label

let create () =
  let events =
    let open CCResult.Infix in
    Pool_common.Message.Field.
      [ Title |> show, [ Data.title ]
      ; Description |> show, [ Data.description ]
      ; WaitingListDisabled |> show, [ "on" ]
      ]
    |> Http_utils.format_request_boolean_values experiment_boolean_fields
    |> ExperimentCommand.Create.decode
    >>= ExperimentCommand.Create.handle
  in
  let expected =
    let open CCResult in
    let open Experiment in
    let* title = Title.create Data.title in
    let* description = Description.create Data.description in
    let waiting_list_disabled = true |> WaitingListDisabled.create in
    let direct_registration_disabled =
      false |> DirectRegistrationDisabled.create
    in
    let registration_disabled = false |> RegistrationDisabled.create in
    let create =
      { title
      ; description
      ; waiting_list_disabled
      ; direct_registration_disabled
      ; registration_disabled
      }
    in
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
      [ Title |> show, [ "" ]; Description |> show, [ Data.description ] ]
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

let create_with_mutually_exclusive_options () =
  let events =
    let open CCResult.Infix in
    Pool_common.Message.Field.
      [ Title |> show, [ Data.title ]
      ; Description |> show, [ Data.description ]
      ; WaitingListDisabled |> show, [ "on" ]
      ; DirectRegistrationDisabled |> show, [ "on" ]
      ]
    |> Http_utils.format_request_boolean_values experiment_boolean_fields
    |> ExperimentCommand.Create.decode
    >>= ExperimentCommand.Create.handle
  in
  let expected = Error Common.Message.(WaitingListFlagsMutuallyExclusive) in
  Alcotest.(
    check
      (result (list Test_utils.event) Test_utils.error)
      "succeeds"
      expected
      events)
;;

let update () =
  let experiment = CCResult.get_exn Data.experiment in
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
  let experiment = CCResult.get_exn Data.experiment in
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
