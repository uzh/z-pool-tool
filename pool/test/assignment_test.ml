open Test_utils
module ContactCommand = Cqrs_command.Contact_command
module AssignmentCommand = Cqrs_command.Assignment_command
module SessionCommand = Cqrs_command.Session_command
module Field = Pool_common.Message.Field

type assignment_data =
  { session : Session.t
  ; experiment : Experiment.t
  ; contact : Contact.t
  }

let assignment_data () =
  let session = Model.create_session () in
  let experiment = Model.create_experiment () in
  let contact = Model.create_contact () in
  { session; experiment; contact }
;;

let confirmation_email experiment assignment =
  let contact = assignment.Assignment.contact in
  let email =
    Contact.(contact |> email_address |> Pool_user.EmailAddress.value)
  in
  let open Message_template in
  let sender = "test@econ.uzh.ch" in
  let ({ email_subject; email_text; _ } : Message_template.t) =
    Model.create_message_template ()
  in
  let email =
    Sihl_email.
      { sender
      ; recipient = email
      ; subject = email_subject |> EmailSubject.value
      ; text = ""
      ; html = Some (email_text |> EmailText.value)
      ; cc = []
      ; bcc = []
      }
  in
  Email.create_job ?smtp_auth_id:experiment.Experiment.smtp_auth_id email
;;

let update_assignment_count_event ~step contact =
  let open Contact in
  contact |> update_num_assignments ~step |> updated |> Pool_event.contact
;;

let create () =
  let { session; experiment; contact } = assignment_data () in
  let confirmation_email = confirmation_email experiment in
  let assignment = Assignment.create contact in
  let events =
    let command =
      AssignmentCommand.Create.
        { contact; session; follow_up_sessions = []; experiment }
    in
    AssignmentCommand.Create.handle command confirmation_email false
  in
  let expected =
    Ok
      [ Email.(Sent (confirmation_email assignment)) |> Pool_event.email
      ; Assignment.(Created (create contact, session.Session.id))
        |> Pool_event.assignment
      ; update_assignment_count_event ~step:1 contact
      ]
  in
  check_result expected events
;;

let create_with_experiment_smtp () =
  let { session; experiment; contact } = assignment_data () in
  let smtp_auth_id = Email.SmtpAuth.Id.create () in
  let assignment = Assignment.create contact in
  let experiment =
    { experiment with Experiment.smtp_auth_id = Some smtp_auth_id }
  in
  let confirmation_email = confirmation_email experiment in
  let events =
    let command =
      AssignmentCommand.Create.
        { contact; session; follow_up_sessions = []; experiment }
    in
    AssignmentCommand.Create.handle command confirmation_email false
  in
  let expected =
    Ok
      [ Email.(Sent (confirmation_email assignment)) |> Pool_event.email
      ; Assignment.(Created (create contact, session.Session.id))
        |> Pool_event.assignment
      ; update_assignment_count_event ~step:1 contact
      ]
  in
  check_result expected events
;;

let canceled () =
  let session = Model.create_session () in
  let assignment = Model.create_assignment () in
  let events = AssignmentCommand.Cancel.handle ([ assignment ], session) in
  let expected =
    Ok
      [ Assignment.Canceled assignment |> Pool_event.assignment
      ; update_assignment_count_event ~step:(-1) assignment.Assignment.contact
      ]
  in
  check_result expected events
;;

let canceled_with_closed_session () =
  let hour = Ptime.Span.of_int_s @@ (60 * 60) in
  let session = Model.create_session () in
  let closed_at = Ptime_clock.now () in
  let session =
    Session.
      { session with
        start =
          Ptime.sub_span (Ptime_clock.now ()) hour
          |> CCOption.get_exn_or "Invalid start"
          |> Start.create
      ; closed_at = Some closed_at
      }
  in
  let assignment = Model.create_assignment () in
  let events = AssignmentCommand.Cancel.handle ([ assignment ], session) in
  let expected =
    Error
      (Pool_common.Message.SessionAlreadyClosed
         (Pool_common.Utils.Time.formatted_date_time closed_at))
  in
  check_result expected events
;;

let set_attendance () =
  let open Assignment in
  let experiment = Model.create_experiment () in
  let assignment =
    Model.create_assignment ~no_show:false ~participated:false ()
  in
  let session = Model.(create_session ~start:(an_hour_ago ()) ()) in
  let increment_num_participaton = IncrementParticipationCount.create false in
  let events =
    SessionCommand.Close.handle
      experiment
      session
      []
      [ assignment, increment_num_participaton, None ]
  in
  let expected =
    let updated_contact =
      let open Contact in
      assignment.contact
      |> update_num_show_ups ~step:1
      |> updated
      |> Pool_event.contact
    in
    Ok
      [ Session.Closed session |> Pool_event.session
      ; Assignment.Updated assignment |> Pool_event.assignment
      ; updated_contact
      ]
  in
  check_result expected events
;;

let set_invalid_attendance () =
  let open Assignment in
  let experiment = Model.create_experiment () in
  let assignment =
    Model.create_assignment ~no_show:true ~participated:true ()
  in
  let session = Model.(create_session ~start:(an_hour_ago ()) ()) in
  let events =
    SessionCommand.Close.handle
      experiment
      session
      []
      [ assignment, IncrementParticipationCount.create false, None ]
  in
  let expected = Error Pool_common.Message.AssignmentsHaveErrors in
  check_result expected events
;;

let assignment_validation () =
  let check_result expected generated =
    Alcotest.(
      check (result unit (list Test_utils.error)) "succeeds" expected generated)
  in
  let experiment = Model.create_experiment () in
  let missing_data_id () =
    let experiment =
      Experiment.
        { experiment with
          external_data_required = ExternalDataRequired.create true
        }
    in
    let assignment =
      Model.create_assignment ~no_show:false ~participated:true ()
    in
    let res = Assignment.validate experiment assignment in
    let expected =
      Error Pool_common.Message.[ FieldRequired Field.ExternalDataId ]
    in
    check_result expected res
  in
  let mutually_exclusive () =
    let assignment =
      Model.create_assignment ~no_show:true ~participated:true ()
    in
    let res = Assignment.validate experiment assignment in
    let expected =
      Error
        Pool_common.Message.[ MutuallyExclusive Field.(NoShow, Participated) ]
    in
    check_result expected res
  in
  missing_data_id ();
  mutually_exclusive ()
;;

let set_attendance_missing_data_id () =
  let open Assignment in
  let experiment = Model.create_experiment () in
  let experiment =
    Experiment.
      { experiment with
        external_data_required = ExternalDataRequired.create true
      }
  in
  let assignment =
    Model.create_assignment ~no_show:true ~participated:true ()
  in
  let session = Model.(create_session ~start:(an_hour_ago ()) ()) in
  let events =
    SessionCommand.Close.handle
      experiment
      session
      []
      [ assignment, IncrementParticipationCount.create false, None ]
  in
  let expected = Error Pool_common.Message.AssignmentsHaveErrors in
  check_result expected events
;;

let set_attendance_with_data_id () =
  let open Assignment in
  let experiment = Model.create_experiment () in
  let experiment =
    Experiment.
      { experiment with
        external_data_required = ExternalDataRequired.create true
      }
  in
  let assignment =
    Model.create_assignment
      ~no_show:false
      ~participated:false
      ~external_data_id:"data-id"
      ()
  in
  let session = Model.(create_session ~start:(an_hour_ago ()) ()) in
  let increment_num_participaton = IncrementParticipationCount.create false in
  let events =
    SessionCommand.Close.handle
      experiment
      session
      []
      [ assignment, increment_num_participaton, None ]
  in
  let expected =
    let updated_contact =
      let open Contact in
      assignment.contact
      |> update_num_show_ups ~step:1
      |> updated
      |> Pool_event.contact
    in
    Ok
      [ Session.Closed session |> Pool_event.session
      ; Assignment.Updated assignment |> Pool_event.assignment
      ; updated_contact
      ]
  in
  check_result expected events
;;

let assign_to_fully_booked_session () =
  let { session; experiment; contact } = assignment_data () in
  let confirmation_email = confirmation_email experiment in
  let session = session |> Model.fully_book_session in
  let events =
    let command =
      AssignmentCommand.Create.
        { contact; session; follow_up_sessions = []; experiment }
    in
    AssignmentCommand.Create.handle command confirmation_email false
  in
  let expected = Error Pool_common.Message.(SessionFullyBooked) in
  check_result expected events
;;

let assign_to_experiment_with_direct_registration_disabled () =
  let { session; experiment; contact } = assignment_data () in
  let confirmation_email = confirmation_email experiment in
  let session = session |> Model.fully_book_session in
  let experiment =
    Experiment.
      { experiment with
        direct_registration_disabled =
          true |> Experiment.DirectRegistrationDisabled.create
      }
  in
  let events =
    let command =
      AssignmentCommand.Create.
        { contact; session; follow_up_sessions = []; experiment }
    in
    AssignmentCommand.Create.handle command confirmation_email false
  in
  let expected = Error Pool_common.Message.(DirectRegistrationIsDisabled) in
  check_result expected events
;;

let assign_to_experiment_with_direct_registration_disabled_as_admin () =
  let { session; experiment; contact } = assignment_data () in
  let confirmation_email = confirmation_email experiment in
  let assignment = Assignment.create contact in
  let experiment =
    Experiment.
      { experiment with
        direct_registration_disabled =
          true |> Experiment.DirectRegistrationDisabled.create
      }
  in
  let events =
    let open AssignmentCommand.Create in
    let command = { contact; session; follow_up_sessions = []; experiment } in
    handle ~direct_enrollment_by_admin:true command confirmation_email false
  in
  let expected =
    Ok
      [ Email.(Sent (confirmation_email assignment)) |> Pool_event.email
      ; Assignment.Created (assignment, session.Session.id)
        |> Pool_event.assignment
      ; update_assignment_count_event ~step:1 contact
      ]
  in
  check_result expected events
;;

let assign_to_session_contact_is_already_assigned () =
  let { session; experiment; contact } = assignment_data () in
  let confirmation_email = confirmation_email experiment in
  let already_assigned = true in
  let events =
    let command =
      AssignmentCommand.Create.
        { contact; session; follow_up_sessions = []; experiment }
    in
    AssignmentCommand.Create.handle command confirmation_email already_assigned
  in
  let expected = Error Pool_common.Message.(AlreadySignedUpForExperiment) in
  check_result expected events
;;

let assign_to_canceled_session () =
  let { session; experiment; contact } = assignment_data () in
  let confirmation_email = confirmation_email experiment in
  let already_assigned = false in
  let canceled_at = Ptime_clock.now () in
  let session = Session.{ session with canceled_at = Some canceled_at } in
  let events =
    let command =
      AssignmentCommand.Create.
        { contact; session; follow_up_sessions = []; experiment }
    in
    AssignmentCommand.Create.handle command confirmation_email already_assigned
  in
  let expected =
    Error
      Pool_common.Message.(
        SessionAlreadyCanceled
          (Pool_common.Utils.Time.formatted_date_time canceled_at))
  in
  Test_utils.check_result expected events
;;

let assign_contact_from_waiting_list () =
  let session = Model.create_session () in
  let experiment = Model.create_experiment () in
  let waiting_list = Model.create_waiting_list () in
  let already_enrolled = false in
  let contact = waiting_list.Waiting_list.contact in
  let assignment = Assignment.create contact in
  let confirmation_email = confirmation_email experiment in
  let events =
    let command =
      AssignmentCommand.CreateFromWaitingList.
        { session; follow_up_sessions = []; waiting_list; already_enrolled }
    in
    AssignmentCommand.CreateFromWaitingList.handle command confirmation_email
  in
  let expected =
    let create =
      Assignment.(create waiting_list.Waiting_list.contact, session.Session.id)
    in
    Ok
      [ Email.(Sent (confirmation_email assignment)) |> Pool_event.email
      ; Assignment.Created create |> Pool_event.assignment
      ; update_assignment_count_event ~step:1 contact
      ]
  in
  check_result expected events
;;

let assign_contact_from_waiting_list_with_follow_ups () =
  let session = Model.create_session () in
  let experiment = Model.create_experiment () in
  let follow_up = Model.create_session ~follow_up_to:session.Session.id () in
  let waiting_list = Model.create_waiting_list () in
  let already_enrolled = false in
  let contact = waiting_list.Waiting_list.contact in
  let assignment = Assignment.create contact in
  let confirmation_email = confirmation_email experiment in
  let events =
    let command =
      AssignmentCommand.CreateFromWaitingList.
        { session
        ; follow_up_sessions = [ follow_up ]
        ; waiting_list
        ; already_enrolled
        }
    in
    AssignmentCommand.CreateFromWaitingList.handle command confirmation_email
  in
  let expected =
    let create_events =
      [ session; follow_up ]
      |> CCList.map (fun session ->
        let create =
          Assignment.(
            create waiting_list.Waiting_list.contact, session.Session.id)
        in
        Assignment.Created create |> Pool_event.assignment)
    in
    Ok
      (((Email.(Sent (confirmation_email assignment)) |> Pool_event.email)
        :: create_events)
       @ [ update_assignment_count_event ~step:2 contact ])
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
  let confirmation_email = confirmation_email experiment in
  let events =
    let command =
      AssignmentCommand.CreateFromWaitingList.
        { session; follow_up_sessions = []; waiting_list; already_enrolled }
    in
    AssignmentCommand.CreateFromWaitingList.handle command confirmation_email
  in
  let expected = Error Pool_common.Message.(RegistrationDisabled) in
  check_result expected events
;;

let assign_to_session_with_follow_ups () =
  let { session; experiment; contact } = assignment_data () in
  let assignment = Assignment.create contact in
  let confirmation_email = confirmation_email experiment in
  let follow_up =
    let base = Model.(create_session ~start:(in_an_hour ())) () in
    Session.
      { base with
        id = Session.Id.create ()
      ; follow_up_to = Some session.Session.id
      }
  in
  let events =
    let command =
      AssignmentCommand.Create.
        { contact; session; follow_up_sessions = [ follow_up ]; experiment }
    in
    AssignmentCommand.Create.handle command confirmation_email false
  in
  let expected =
    let sessions = [ session; follow_up ] in
    let create_events =
      sessions
      |> CCList.map (fun session ->
        Assignment.(Created (create contact, session.Session.id))
        |> Pool_event.assignment)
    in
    let increase_num_events =
      update_assignment_count_event ~step:(CCList.length sessions) contact
    in
    let email_event =
      [ Email.Sent (confirmation_email assignment) |> Pool_event.email ]
    in
    email_event @ create_events @ [ increase_num_events ] |> CCResult.return
  in
  check_result expected events
;;

let mark_uncanceled_as_deleted () =
  let open Assignment in
  let assignment = Model.create_assignment () in
  let assignment = { assignment with canceled_at = None } in
  let events =
    AssignmentCommand.MarkAsDeleted.handle
      ( assignment.contact
      , [ assignment ]
      , IncrementParticipationCount.create false )
  in
  let expected =
    let open Contact in
    let contact = assignment.contact in
    let num_assignments =
      NumberOfAssignments.update (-1) contact.num_assignments
    in
    Ok
      [ Contact.Updated { contact with num_assignments } |> Pool_event.contact
      ; MarkedAsDeleted assignment |> Pool_event.assignment
      ]
  in
  check_result expected events
;;

let marked_canceled_as_deleted () =
  let open Assignment in
  let assignment = Model.create_assignment () in
  let assignment =
    { assignment with canceled_at = Some (CanceledAt.create_now ()) }
  in
  let events =
    AssignmentCommand.MarkAsDeleted.handle
      ( assignment.contact
      , [ assignment ]
      , IncrementParticipationCount.create false )
  in
  let expected =
    Ok
      [ Contact.Updated assignment.contact |> Pool_event.contact
      ; MarkedAsDeleted assignment |> Pool_event.assignment
      ]
  in
  check_result expected events
;;

let marked_closed_with_followups_as_deleted () =
  let open Assignment in
  let open Contact in
  let assignment = Model.create_assignment () in
  let assignment =
    { assignment with
      no_show = false |> NoShow.create |> CCOption.return
    ; participated = true |> Participated.create |> CCOption.return
    }
  in
  let follow_up = Model.create_assignment () in
  let contact =
    let num_assignments = NumberOfAssignments.of_int 3 in
    let num_show_ups = NumberOfShowUps.of_int 2 in
    let num_participations = NumberOfParticipations.of_int 1 in
    { assignment.contact with
      num_assignments
    ; num_show_ups
    ; num_participations
    }
  in
  let events =
    AssignmentCommand.MarkAsDeleted.handle
      ( contact
      , [ assignment; follow_up ]
      , IncrementParticipationCount.create true )
  in
  let expected =
    let { num_assignments; num_show_ups; num_participations; _ } = contact in
    let contact =
      { contact with
        num_assignments = NumberOfAssignments.update (-2) num_assignments
      ; num_show_ups = NumberOfShowUps.update (-1) num_show_ups
      ; num_participations =
          NumberOfParticipations.update (-1) num_participations
      }
    in
    Ok
      [ Contact.Updated contact |> Pool_event.contact
      ; MarkedAsDeleted assignment |> Pool_event.assignment
      ; MarkedAsDeleted follow_up |> Pool_event.assignment
      ]
  in
  check_result expected events
;;

let cancel_deleted_assignment () =
  let session = Model.(create_session ~start:(an_hour_ago ()) ()) in
  let assignment = Model.create_assignment () in
  let assignment =
    Assignment.
      { assignment with marked_as_deleted = MarkedAsDeleted.create true }
  in
  let events = AssignmentCommand.Cancel.handle ([ assignment ], session) in
  let expected =
    Error Pool_common.Message.(IsMarkedAsDeleted Field.Assignment)
  in
  check_result expected events
;;

let send_reminder_invalid () =
  let open Cqrs_command.Assignment_command.SendReminder in
  let open Test_utils in
  let experiment = Model.create_experiment () in
  let session = Model.create_session () in
  let assignment = Model.create_assignment () in
  let create_email _ =
    Ok
      (Model.create_email_job
         ?smtp_auth_id:experiment.Experiment.smtp_auth_id
         ())
  in
  let create_tet_message _ cell_phone =
    Ok (Model.create_text_message cell_phone)
  in
  let channel = Pool_common.Reminder.Channel.Email in
  let assignment1 =
    Assignment.
      { assignment with marked_as_deleted = MarkedAsDeleted.(create true) }
  in
  let assignment2 =
    Assignment.{ assignment with canceled_at = Some (CanceledAt.create_now ()) }
  in
  let handle assignment =
    handle (create_email, create_tet_message) session assignment channel
  in
  let res1 = handle assignment1 in
  let () =
    check_result
      (Error Pool_common.Message.(IsMarkedAsDeleted Field.Assignment))
      res1
  in
  let res2 = handle assignment2 in
  let () = check_result (Error Pool_common.Message.AssignmentIsCanceled) res2 in
  ()
;;

module SwapSessionData = struct
  open Message_template
  open Pool_common

  let language = Language.En
  let email_subject = "Change" |> EmailSubject.of_string
  let email_text = "<p>Text</p>" |> EmailText.of_string
  let plain_text = "Text" |> PlainText.of_string

  let urlencoded notify_user session_id =
    Message.Field.
      [ show Session, [ Session.Id.value session_id ]
      ; show NotifyContact, [ NotifyContact.stringify notify_user ]
      ; show Language, [ Pool_common.Language.show Language.En ]
      ; show EmailSubject, [ EmailSubject.value email_subject ]
      ; show EmailText, [ EmailText.value email_text ]
      ; show PlainText, [ PlainText.value plain_text ]
      ]
  ;;
end

let swap_session_without_notification () =
  let current_session = Model.(create_session ~start:(in_an_hour ()) ()) in
  let new_session = Model.(create_session ~start:(in_an_hour ()) ()) in
  let assignment = Model.create_assignment () in
  let assignment_id = Assignment.Id.create () in
  let result =
    let open AssignmentCommand.SwapSession in
    handle ~assignment_id ~current_session ~new_session assignment None
  in
  let expected =
    Assignment.
      [ MarkedAsDeleted assignment
      ; Created ({ assignment with id = assignment_id }, new_session.Session.id)
      ]
    |> CCList.map Pool_event.assignment
    |> CCResult.return
  in
  check_result expected result
;;

let swap_session_with_notification () =
  let current_session = Model.(create_session ~start:(in_an_hour ()) ()) in
  let new_session = Model.(create_session ~start:(in_an_hour ()) ()) in
  let assignment = Model.create_assignment () in
  let assignment_id = Assignment.Id.create () in
  let msg = Model.create_email_job () in
  let result =
    let open AssignmentCommand.SwapSession in
    handle ~assignment_id ~current_session ~new_session assignment (Some msg)
  in
  let expected =
    Assignment.
      [ MarkedAsDeleted assignment |> Pool_event.assignment
      ; Created ({ assignment with id = assignment_id }, new_session.Session.id)
        |> Pool_event.assignment
      ; Email.Sent msg |> Pool_event.email
      ]
    |> CCResult.return
  in
  check_result expected result
;;

let swap_session_to_past_session () =
  let current_session = Model.(create_session ~start:(an_hour_ago ()) ()) in
  let new_session = Model.(create_session ~start:(an_hour_ago ()) ()) in
  let assignment = Model.create_assignment () in
  let assignment_id = Assignment.Id.create () in
  let result =
    let open AssignmentCommand.SwapSession in
    handle ~assignment_id ~current_session ~new_session assignment None
  in
  let expected =
    Assignment.
      [ MarkedAsDeleted assignment
      ; Created ({ assignment with id = assignment_id }, new_session.Session.id)
      ]
    |> CCList.map Pool_event.assignment
    |> CCResult.return
  in
  check_result expected result
;;

(* Integration tests *)

let cancel_assignment_with_follow_ups _ () =
  let open Utils.Lwt_result.Infix in
  let%lwt experiment = Repo.create_experiment () in
  let%lwt contact =
    Integration_utils.ContactRepo.create ~with_terms_accepted:true ()
  in
  let%lwt location = Repo.first_location () in
  (* Save sessions in Database *)
  let create_session ?parent_id start =
    let session =
      Model.(create_session ?follow_up_to:parent_id ~start ~location ())
    in
    Session.Created (session, experiment.Experiment.id)
    |> Pool_event.session
    |> Pool_event.handle_event Data.database_label
  in
  let%lwt parent_session =
    let%lwt () = create_session (Model.in_an_hour ()) in
    Session.find_all_for_experiment Data.database_label experiment.Experiment.id
    ||> CCList.hd
  in
  let%lwt () =
    create_session ~parent_id:parent_session.Session.id (Model.in_two_hours ())
  in
  let%lwt sessions =
    let%lwt session =
      Session.find Data.database_label parent_session.Session.id
      ||> get_or_failwith
    in
    let%lwt follow_ups =
      Session.find_follow_ups Data.database_label session.Session.id
    in
    Lwt.return (session :: follow_ups)
  in
  (* Create assignments *)
  let%lwt () =
    sessions
    |> CCList.map Session.to_public
    |> CCList.map (fun session ->
      Assignment.(Created (create contact, session.Session.Public.id))
      |> Pool_event.assignment)
    |> Pool_event.handle_events Data.database_label
  in
  (* Cancel assignments *)
  let%lwt () =
    let open Assignment in
    let%lwt assignment_id =
      find_all_by_experiment_and_contact_opt
        Data.database_label
        experiment.Experiment.id
        contact
      ||> CCList.hd
      ||> fun ({ Public.id; _ } : Public.t) -> id |> Id.value |> Id.of_string
    in
    let%lwt assignments =
      find_with_follow_ups Data.database_label assignment_id
    in
    AssignmentCommand.Cancel.handle (assignments, parent_session)
    |> get_or_failwith
    |> Pool_event.handle_events Data.database_label
  in
  (* Expect all assigments to be canceled *)
  let%lwt res =
    Assignment.find_all_by_experiment_and_contact_opt
      Data.database_label
      experiment.Experiment.id
      contact
    ||> CCList.filter (fun { Assignment.Public.canceled_at; _ } ->
      CCOption.is_none canceled_at)
    ||> CCList.is_empty
  in
  let () = Alcotest.(check bool "succeeds" true res) in
  Lwt.return_unit
;;
