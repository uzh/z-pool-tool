module ContactCommand = Cqrs_command.Contact_command
module AssignmentCommand = Cqrs_command.Assignment_command
module Field = Pool_common.Message.Field
module Model = Test_utils.Model

type assignment_data =
  { session : Session.Public.t
  ; experiment : Experiment.t
  ; contact : Contact.t
  }

let assignment_data () =
  let session = Model.create_public_session () in
  let experiment = Model.create_experiment () in
  let contact = Model.create_contact () in
  { session; experiment; contact }
;;

let confirmation_email =
  let language = Pool_common.Language.En in
  let subject = "Confirmation" |> I18n.Content.create |> CCResult.get_exn in
  let text = "Text" |> I18n.Content.create |> CCResult.get_exn in
  let session_text = "Session info" in
  Email.{ subject; text; language; session_text }
;;

let create () =
  let { session; experiment; contact } = assignment_data () in
  let waiting_list =
    Model.create_waiting_list_from_experiment_and_contact experiment contact
  in
  let experiment = experiment |> Model.experiment_to_public_experiment in
  let events =
    let command =
      AssignmentCommand.Create.
        { contact; session; waiting_list = Some waiting_list; experiment }
    in
    AssignmentCommand.Create.handle command confirmation_email false
  in
  let expected =
    Ok
      [ Waiting_list.Deleted waiting_list |> Pool_event.waiting_list
      ; Assignment.(Created { contact; session_id = session.Session.Public.id })
        |> Pool_event.assignment
      ; Email.(
          AssignmentConfirmationSent
            (waiting_list.Waiting_list.contact.Contact.user, confirmation_email))
        |> Pool_event.email
      ]
  in
  Test_utils.check_result expected events
;;

let canceled () =
  let assignment = Model.create_assignment () in
  let events = AssignmentCommand.Cancel.handle assignment in
  let expected =
    Ok [ Assignment.Canceled assignment |> Pool_event.assignment ]
  in
  Test_utils.check_result expected events
;;

let set_attendance () =
  let open Assignment in
  let assignment = Model.create_assignment () in
  let session = Model.create_session () in
  let show_up = true |> ShowUp.create in
  let participated = false |> Participated.create in
  let events =
    AssignmentCommand.SetAttendance.handle
      session
      [ assignment, show_up, participated ]
  in
  let expected =
    Ok
      [ Session.Closed session |> Pool_event.session
      ; Assignment.AttendanceSet (assignment, show_up, participated)
        |> Pool_event.assignment
      ; Contact.ShowUpIncreased assignment.contact |> Pool_event.contact
      ]
  in
  Test_utils.check_result expected events
;;

let set_invalid_attendance () =
  let open Assignment in
  let assignment = Model.create_assignment () in
  let session = Model.create_session () in
  let show_up = false |> ShowUp.create in
  let participated = true |> Participated.create in
  let events =
    AssignmentCommand.SetAttendance.handle
      session
      [ assignment, show_up, participated ]
  in
  let expected =
    Error
      Pool_common.Message.(FieldRequiresCheckbox Field.(Participated, ShowUp))
  in
  Test_utils.check_result expected events
;;

let assign_to_fully_booked_session () =
  let { session; experiment; contact } = assignment_data () in
  let session = session |> Model.fully_book_public_session in
  let waiting_list =
    Model.create_waiting_list_from_experiment_and_contact experiment contact
  in
  let experiment = experiment |> Model.experiment_to_public_experiment in
  let events =
    let command =
      AssignmentCommand.Create.
        { contact; session; waiting_list = Some waiting_list; experiment }
    in
    AssignmentCommand.Create.handle command confirmation_email false
  in
  let expected = Error Pool_common.Message.(SessionFullyBooked) in
  Test_utils.check_result expected events
;;

let assign_to_experiment_with_direct_registration_disabled () =
  let { session; experiment; contact } = assignment_data () in
  let session = session |> Model.fully_book_public_session in
  let waiting_list =
    Model.create_waiting_list_from_experiment_and_contact experiment contact
  in
  let experiment =
    let public = experiment |> Model.experiment_to_public_experiment in
    Experiment.Public.
      { public with
        direct_registration_disabled =
          true |> Experiment.DirectRegistrationDisabled.create
      }
  in
  let events =
    let command =
      AssignmentCommand.Create.
        { contact; session; waiting_list = Some waiting_list; experiment }
    in
    AssignmentCommand.Create.handle command confirmation_email false
  in
  let expected = Error Pool_common.Message.(DirectRegistrationIsDisabled) in
  Test_utils.check_result expected events
;;

let assign_to_session_contact_is_already_assigned () =
  let { session; experiment; contact } = assignment_data () in
  let already_assigned = true in
  let waiting_list =
    Model.create_waiting_list_from_experiment_and_contact experiment contact
  in
  let experiment = experiment |> Model.experiment_to_public_experiment in
  let events =
    let command =
      AssignmentCommand.Create.
        { contact; session; waiting_list = Some waiting_list; experiment }
    in
    AssignmentCommand.Create.handle command confirmation_email already_assigned
  in
  let expected = Error Pool_common.Message.(AlreadySignedUpForExperiment) in
  Test_utils.check_result expected events
;;

let assign_contact_from_waiting_list () =
  let session = Model.create_session () in
  let waiting_list = Model.create_waiting_list () in
  let already_enrolled = false in
  let events =
    let command =
      AssignmentCommand.CreateFromWaitingList.
        { session; waiting_list; already_enrolled }
    in
    AssignmentCommand.CreateFromWaitingList.handle command confirmation_email
  in
  let expected =
    let create =
      Assignment.
        { contact = waiting_list.Waiting_list.contact
        ; session_id = session.Session.id
        }
    in
    Ok
      [ Waiting_list.Deleted waiting_list |> Pool_event.waiting_list
      ; Assignment.Created create |> Pool_event.assignment
      ; Email.(
          AssignmentConfirmationSent
            (waiting_list.Waiting_list.contact.Contact.user, confirmation_email))
        |> Pool_event.email
      ]
  in
  Test_utils.check_result expected events
;;

let assign_contact_from_waiting_list_to_disabled_experiment () =
  let session = Model.create_session () in
  let experiment = Model.create_experiment () in
  let experiment =
    Experiment.
      { experiment with
        registration_disabled = true |> RegistrationDisabled.create
      }
  in
  let waiting_list = Model.create_waiting_list () in
  let waiting_list = Waiting_list.{ waiting_list with experiment } in
  let already_enrolled = false in
  let events =
    let command =
      AssignmentCommand.CreateFromWaitingList.
        { session; waiting_list; already_enrolled }
    in
    AssignmentCommand.CreateFromWaitingList.handle command confirmation_email
  in
  let expected = Error Pool_common.Message.(RegistrationDisabled) in
  Test_utils.check_result expected events
;;
