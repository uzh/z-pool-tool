module ExperimentCommand = Cqrs_command.Experiment_command
module Common = Pool_common
module Model = Test_utils.Model

let experiment_boolean_fields =
  Experiment.boolean_fields |> CCList.map Pool_common.Message.Field.show
;;

module Data = struct
  let title = "New experiment"
  let public_title = "public_experiment_title"
  let description = "Description"

  module Filter = struct
    open Filter

    let single_filter : filter = PredS (Key.Paused, Operator.Equal, Bool false)

    let or_filter : filter =
      Or (PredS (Key.Name, Operator.Equal, Str "foo"), single_filter)
    ;;

    let list_filter : filter =
      PredM (Key.Age, Operator.ContainsNone, Lst [ Nr 20.0; Nr 21.0 ])
    ;;

    let and_filter : filter = And (or_filter, list_filter)
    let t = create and_filter
  end

  let filter = Some Filter.t

  let experiment =
    let open CCResult in
    let id = Pool_common.Id.create () in
    let* title = title |> Experiment.Title.create in
    let* public_title = public_title |> Experiment.PublicTitle.create in
    let* description = description |> Experiment.Description.create in
    Ok
      Experiment.
        { id
        ; title
        ; public_title
        ; description
        ; filter
        ; direct_registration_disabled =
            false |> DirectRegistrationDisabled.create
        ; registration_disabled = false |> RegistrationDisabled.create
        ; allow_uninvited_signup = false |> AllowUninvitedSignup.create
        ; experiment_type = Some Pool_common.ExperimentType.Lab
        ; invitation_template = None
        ; session_reminder_subject = None
        ; session_reminder_text = None
        ; session_reminder_lead_time = None
        ; created_at = Common.CreatedAt.create ()
        ; updated_at = Common.UpdatedAt.create ()
        }
  ;;
end

let database_label = Test_utils.Data.database_label

let create () =
  let experiment = Model.create_experiment () in
  let events = Ok [ Experiment.Created experiment |> Pool_event.experiment ] in
  let expected =
    Ok [ Experiment.Created experiment |> Pool_event.experiment ]
  in
  Test_utils.check_result expected events
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
  Test_utils.check_result expected events
;;

let update () =
  let experiment = Model.create_experiment () in
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
  Test_utils.check_result expected events
;;

let delete_with_sessions () =
  let experiment = Model.create_experiment () in
  let events =
    let session_count = 1234 in
    ExperimentCommand.Delete.(
      handle { experiment_id = experiment.Experiment.id; session_count })
  in
  let expected = Error Pool_common.Message.ExperimentSessionCountNotZero in
  Test_utils.check_result expected events
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
  Test_utils.check_result expected events
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
