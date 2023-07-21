open Test_utils
open Cqrs_command
open Utils.Lwt_result.Infix

let database_label = Data.database_label
let get_exn = get_or_failwith_pool_error

let get_contact contact_id =
  contact_id |> Contact.find database_label |> Lwt.map get_exn
;;

let get_session session_id =
  session_id |> Session.find database_label |> Lwt.map get_exn
;;

let find_assignment_by_contact_and_session contact_id session_id =
  let open Assignment in
  find_uncanceled_by_session database_label session_id
  >|+ CCList.find (fun ({ contact; _ } : Assignment.t) ->
    Contact.(Id.equal (id contact) contact_id))
  ||> get_exn
;;

let set_sessions_to_past session_ids =
  let open Session in
  session_ids
  |> Lwt_list.map_s (fun id -> find database_label id)
  ||> CCResult.flatten_l
  >|+ CCList.map (fun (session : t) ->
    let session = { session with start = Model.an_hour_ago () } in
    Updated (Model.session_to_session_base session, session.location, session)
    |> Pool_event.session)
  |>> Pool_event.handle_events database_label
  ||> get_exn
;;

let sign_up_for_session experiment contact session_id =
  let%lwt sessions =
    Session.find_open_with_follow_ups database_label session_id
    >|+ CCList.map Session.to_public
    ||> get_exn
  in
  let email = Model.create_email () in
  Assignment_command.Create.(handle { contact; sessions; experiment })
    email
    false
  |> get_exn
  |> Pool_event.handle_events database_label
;;

let close_session
  ?(no_show = false)
  ?(participated = true)
  session
  contact_id
  experiment_id
  =
  let open Assignment in
  let open Assignment_command in
  let%lwt assignment =
    find_assignment_by_contact_and_session contact_id session.Session.id
  in
  let no_show = no_show |> NoShow.create in
  let participated = participated |> Participated.create in
  let%lwt increment_num_participations =
    contact_participation_in_other_assignments
      database_label
      [ assignment ]
      experiment_id
      contact_id
    >|+ not
    >|+ IncrementParticipationCount.create
    ||> get_exn
  in
  (assignment, no_show, participated, increment_num_participations, None)
  |> CCList.pure
  |> SetAttendance.handle session
  |> get_exn
  |> Pool_event.handle_events database_label
;;

let delete_assignment experiment_id contact assignments =
  let open Assignment_command in
  let%lwt decrement_num_participations =
    Assignment.(
      contact_participation_in_other_assignments
        database_label
        assignments
        experiment_id
        (Contact.id contact)
      >|+ not
      >|+ IncrementParticipationCount.create
      ||> get_exn)
  in
  (contact, assignments, decrement_num_participations)
  |> MarkAsDeleted.handle
  |> get_exn
  |> Pool_event.handle_events database_label
;;

let initialize contact_id experiment_id session_id ?followup_session_id () =
  let open Integration_utils in
  let%lwt contact =
    ContactRepo.create ~id:contact_id ~with_terms_accepted:true ()
  in
  let%lwt experiment = ExperimentRepo.create ~id:experiment_id () in
  let%lwt session = SessionRepo.create ~id:session_id experiment_id () in
  let%lwt follow_up_session =
    followup_session_id
    |> CCOption.map_or ~default:Lwt.return_none (fun id ->
      SessionRepo.create ~id ~follow_up_to:session_id experiment_id ()
      |> Lwt.map CCOption.return)
  in
  Lwt.return (contact, experiment, session, follow_up_session)
;;

module AttendAll = struct
  let contact_id = Contact.Id.create ()
  let session_id = Session.Id.create ()
  let followup_session_id = Session.Id.create ()
  let experiment_id = Experiment.Id.create ()

  let initialize =
    initialize contact_id experiment_id session_id ~followup_session_id
  ;;

  let register_for_session _ () =
    let%lwt contact, experiment, _, _ = initialize () in
    let%lwt () = sign_up_for_session experiment contact session_id in
    let%lwt res = get_contact contact_id in
    let%lwt expected =
      Lwt.return
        Contact.{ res with num_assignments = NumberOfAssignments.of_int 2 }
    in
    let () = Alcotest.(check Test_utils.contact "succeeds" expected res) in
    let%lwt () = set_sessions_to_past [ session_id; followup_session_id ] in
    Lwt.return_unit
  ;;

  let close_first_session _ () =
    let%lwt session = get_session session_id in
    let%lwt () = close_session session contact_id experiment_id in
    let%lwt res = get_contact contact_id in
    let contact =
      Contact.
        { res with
          num_show_ups = NumberOfShowUps.of_int 1
        ; num_participations = NumberOfParticipations.of_int 1
        }
    in
    let () = Alcotest.(check Test_utils.contact "succeeds" contact res) in
    Lwt.return_unit
  ;;

  let close_follow_up_session _ () =
    let%lwt follow_up = get_session followup_session_id in
    let%lwt () = close_session follow_up contact_id experiment_id in
    let%lwt res = get_contact contact_id in
    let contact =
      Contact.{ res with num_show_ups = NumberOfShowUps.of_int 2 }
    in
    let () = Alcotest.(check Test_utils.contact "succeeds" contact res) in
    Lwt.return_unit
  ;;
end

module CancelSession = struct
  let initialize ?followup_session_id u =
    initialize
      ?followup_session_id
      (Contact.Id.create u)
      (Experiment.Id.create u)
      (Session.Id.create u)
      ()
  ;;

  let test_cancellation experiment contact_id initial_nr_assignments test_cases =
    let%lwt contact = get_contact contact_id in
    let test_result expected_nr_assignments =
      let%lwt res = get_contact contact_id in
      let contact =
        Contact.
          { contact with
            num_assignments = NumberOfAssignments.of_int expected_nr_assignments
          }
      in
      let () = Alcotest.(check Test_utils.contact "succeeds" contact res) in
      Lwt.return_unit
    in
    let%lwt () = test_result initial_nr_assignments in
    test_cases
    |> Lwt_list.iter_s (fun (session, expected_nr_assignments) ->
      let%lwt () =
        let open Cqrs_command.Session_command.Cancel in
        let%lwt follow_ups =
          Session.find_follow_ups database_label session.Session.id ||> get_exn
        in
        let%lwt assignments =
          session :: follow_ups
          |> Lwt_list.fold_left_s
               (fun assignments session ->
                 Assignment.find_uncanceled_by_session
                   database_label
                   session.Session.id
                 ||> get_exn
                 ||> CCList.append assignments)
               []
          ||> Assignment.group_by_contact
        in
        let email = Model.create_email () in
        let reason = "Some reason" |> Session.CancellationReason.of_string in
        handle
          (session :: follow_ups)
          experiment
          assignments
          (fun _ _ -> Ok email)
          Session_test.create_cancellation_text_message
          [ Pool_common.NotifyVia.Email ]
          reason
        |> get_exn
        |> Pool_event.handle_events database_label
      in
      test_result expected_nr_assignments)
  ;;

  let without_followups _ () =
    let%lwt contact, experiment, session, _ = initialize () in
    let%lwt () = sign_up_for_session experiment contact session.Session.id in
    test_cancellation experiment (Contact.id contact) 1 [ session, 0 ]
  ;;

  let follow_up _ () =
    let%lwt contact, experiment, session, followup_session =
      initialize ~followup_session_id:(Session.Id.create ()) ()
    in
    let followup_session =
      followup_session |> CCOption.get_exn_or "Follow up session is required"
    in
    let%lwt () = sign_up_for_session experiment contact session.Session.id in
    test_cancellation
      experiment
      (Contact.id contact)
      2
      [ followup_session, 1; session, 0 ]
  ;;

  let main_with_follow_up _ () =
    let%lwt contact, experiment, session, _ =
      initialize ~followup_session_id:(Session.Id.create ()) ()
    in
    let%lwt () = sign_up_for_session experiment contact session.Session.id in
    test_cancellation experiment (Contact.id contact) 2 [ session, 0 ]
  ;;
end

module DoNotAttend = struct
  let contact_id = Contact.Id.create ()
  let session_id = Session.Id.create ()
  let experiment_id = Experiment.Id.create ()
  let initialize = initialize contact_id experiment_id session_id

  let register_for_session _ () =
    let%lwt contact, experiment, _, _ = initialize () in
    let%lwt () = sign_up_for_session experiment contact session_id in
    let%lwt res = get_contact contact_id in
    let%lwt expected =
      Lwt.return
        Contact.{ res with num_assignments = NumberOfAssignments.of_int 1 }
    in
    let () = Alcotest.(check Test_utils.contact "succeeds" expected res) in
    let%lwt () = set_sessions_to_past [ session_id ] in
    Lwt.return_unit
  ;;

  let close_main _ () =
    let%lwt session = get_session session_id in
    let%lwt () =
      close_session ~participated:false session contact_id experiment_id
    in
    let%lwt res = get_contact contact_id in
    let contact =
      Contact.
        { res with
          num_assignments = NumberOfAssignments.of_int 1
        ; num_show_ups = NumberOfShowUps.of_int 1
        ; num_participations = NumberOfParticipations.of_int 1
        }
    in
    let () = Alcotest.(check Test_utils.contact "succeeds" contact res) in
    Lwt.return_unit
  ;;
end

module NoShow = struct
  let contact_id = Contact.Id.create ()
  let session_id = Session.Id.create ()
  let experiment_id = Experiment.Id.create ()
  let initialize = initialize contact_id experiment_id session_id

  let register_for_session _ () =
    let%lwt contact, experiment, _, _ = initialize () in
    let%lwt () = sign_up_for_session experiment contact session_id in
    let%lwt res = get_contact contact_id in
    let%lwt expected =
      Lwt.return
        Contact.{ res with num_assignments = NumberOfAssignments.of_int 1 }
    in
    let () = Alcotest.(check Test_utils.contact "succeeds" expected res) in
    let%lwt () = set_sessions_to_past [ session_id ] in
    Lwt.return_unit
  ;;

  let close_main _ () =
    let%lwt session = get_session session_id in
    let%lwt () =
      close_session
        ~no_show:true
        ~participated:false
        session
        contact_id
        experiment_id
    in
    let%lwt res = get_contact contact_id in
    let contact =
      Contact.
        { res with
          num_assignments = NumberOfAssignments.of_int 1
        ; num_no_shows = NumberOfNoShows.of_int 1
        ; num_show_ups = NumberOfShowUps.of_int 0
        ; num_participations = NumberOfParticipations.of_int 0
        }
    in
    let () = Alcotest.(check Test_utils.contact "succeeds" contact res) in
    Lwt.return_unit
  ;;
end

module DeleteAttended = struct
  let contact_id = AttendAll.contact_id
  let experiment_id = AttendAll.experiment_id
  let session_id = AttendAll.session_id
  let followup_session_id = AttendAll.followup_session_id

  let delete_follow_up _ () =
    let%lwt follow_up =
      find_assignment_by_contact_and_session contact_id followup_session_id
    in
    let%lwt contact = get_contact contact_id in
    let%lwt () = delete_assignment experiment_id contact [ follow_up ] in
    let contact =
      Contact.(
        contact
        |> update_num_assignments ~step:(-1)
        |> update_num_show_ups ~step:(-1))
    in
    let%lwt res = get_contact contact_id in
    let () = Alcotest.(check Test_utils.contact "succeeds" contact res) in
    Lwt.return_unit
  ;;

  let delete_main _ () =
    let%lwt session =
      find_assignment_by_contact_and_session contact_id session_id
    in
    let%lwt contact = get_contact contact_id in
    let%lwt () = delete_assignment experiment_id contact [ session ] in
    let contact =
      Contact.(
        contact
        |> update_num_assignments ~step:(-1)
        |> update_num_show_ups ~step:(-1)
        |> update_num_participations ~step:(-1))
    in
    let%lwt res = get_contact contact_id in
    let () = Alcotest.(check Test_utils.contact "succeeds" contact res) in
    Lwt.return_unit
  ;;
end

module DeleteUnattended = struct
  let contact_id = Contact.Id.create ()
  let session_id = Session.Id.create ()
  let followup_session_id = Session.Id.create ()
  let experiment_id = Experiment.Id.create ()

  let initialize =
    initialize contact_id experiment_id session_id ~followup_session_id
  ;;

  let num_assignments = 4

  let register_for_session _ () =
    let%lwt contact, experiment, _, _ = initialize () in
    let contact =
      Contact.
        { contact with
          num_participations = NumberOfParticipations.of_int 2
        ; num_no_shows = NumberOfNoShows.of_int 2
        ; num_show_ups = NumberOfShowUps.of_int 2
        ; num_assignments = NumberOfAssignments.of_int num_assignments
        }
    in
    let%lwt () =
      Contact.Updated contact
      |> Pool_event.contact
      |> Pool_event.handle_event database_label
    in
    let%lwt () = sign_up_for_session experiment contact session_id in
    let%lwt res = get_contact contact_id in
    let%lwt expected =
      Lwt.return
        Contact.
          { res with
            num_assignments = NumberOfAssignments.of_int (num_assignments + 2)
          }
    in
    let () = Alcotest.(check Test_utils.contact "succeeds" expected res) in
    let%lwt () = set_sessions_to_past [ session_id; followup_session_id ] in
    Lwt.return_unit
  ;;

  let delete_main _ () =
    let%lwt contact = get_contact contact_id in
    let%lwt assignments =
      [ session_id; followup_session_id ]
      |> Lwt_list.map_s (find_assignment_by_contact_and_session contact_id)
    in
    let%lwt () = delete_assignment experiment_id contact assignments in
    let%lwt res = get_contact contact_id in
    let expected =
      Contact.
        { res with
          num_participations = NumberOfParticipations.of_int 2
        ; num_no_shows = NumberOfNoShows.of_int 2
        ; num_show_ups = NumberOfShowUps.of_int 2
        ; num_assignments = NumberOfAssignments.of_int num_assignments
        }
    in
    let () = Alcotest.(check Test_utils.contact "succeeds" expected res) in
    let%lwt () = set_sessions_to_past [ session_id; followup_session_id ] in
    Lwt.return_unit
  ;;
end
