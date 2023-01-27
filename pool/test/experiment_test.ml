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

    let single_query : query =
      Pred
        (Predicate.create
           Key.(Hardcoded Firstname)
           Operator.Equal
           (Single (Str "Foo")))
    ;;

    let or_query : query =
      Or
        [ Pred
            (Predicate.create
               Key.(Hardcoded Name)
               Operator.Equal
               (Single (Str "Bar")))
        ; single_query
        ]
    ;;

    let list_query : query =
      Pred
        (Predicate.create
           Key.(Hardcoded Name)
           Operator.ContainsNone
           (Lst [ Str "foo"; Str "bar" ]))
    ;;

    let and_query : query = And [ or_query; list_query ]
    let t = create None and_query
  end

  let filter = Some Filter.t

  let experiment =
    let open CCResult in
    let open Experiment in
    let* title = title |> Title.create in
    let* public_title = public_title |> PublicTitle.create in
    let* description = description |> Description.create in
    Ok
      { id = Id.create ()
      ; title
      ; public_title
      ; description
      ; filter
      ; direct_registration_disabled =
          false |> DirectRegistrationDisabled.create
      ; registration_disabled = false |> RegistrationDisabled.create
      ; allow_uninvited_signup = false |> AllowUninvitedSignup.create
      ; experiment_type = Some Pool_common.ExperimentType.Lab
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
  let expected = Error Common.Message.(Conformist [ Field.Title, NoValue ]) in
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
    ExperimentCommand.Delete.(handle { experiment; session_count })
  in
  let expected = Error Pool_common.Message.ExperimentSessionCountNotZero in
  Test_utils.check_result expected events
;;

let delete_with_filter () =
  let experiment = Model.create_experiment () in
  let filter = Filter.create None (Filter_test.nr_of_siblings ()) in
  let experiment = Experiment.{ experiment with filter = Some filter } in
  let events =
    let session_count = 0 in
    ExperimentCommand.Delete.(handle { experiment; session_count })
  in
  let expected =
    Ok
      [ Experiment.Destroyed experiment.Experiment.id |> Pool_event.experiment
      ; Filter.Deleted filter |> Pool_event.filter
      ]
  in
  Test_utils.check_result expected events
;;
