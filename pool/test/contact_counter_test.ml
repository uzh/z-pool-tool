open Test_utils
open Cqrs_command

let database_label = Data.database_label
let get_exn = get_or_failwith_pool_error

let get_contact contact_id =
  contact_id |> Contact.find database_label |> Lwt.map get_exn
;;

let get_session session_id =
  session_id |> Session.find database_label |> Lwt.map get_exn
;;

let find_assignment_by_contact_and_session contact_id session_id =
  let open Utils.Lwt_result.Infix in
  let open Assignment in
  find_uncanceled_by_session database_label session_id
  >|+ CCList.find (fun ({ contact; _ } : Assignment.t) ->
        Contact.(Id.equal (id contact) contact_id))
  ||> get_exn
;;

let set_sessions_to_past session_ids =
  let open Utils.Lwt_result.Infix in
  let open Session in
  session_ids
  |> Lwt_list.map_s (fun id -> find database_label id)
  ||> CCResult.flatten_l
  >|+ CCList.map (fun (session : t) ->
        let session = { session with start = Model.an_hour_ago () } in
        Updated
          (Model.session_to_session_base session, session.location, session)
        |> Pool_event.session)
  |>> Pool_event.handle_events database_label
  ||> get_exn
;;

let sign_up_for_session experiment contact session_id =
  let open Utils.Lwt_result.Infix in
  let experiment = experiment |> Model.experiment_to_public_experiment in
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
  let open Utils.Lwt_result.Infix in
  let open Assignment in
  let open Assignment_command in
  let%lwt assignment =
    find_assignment_by_contact_and_session contact_id session.Session.id
  in
  let no_show = no_show |> NoShow.create in
  let participated = participated |> Participated.create in
  let%lwt increment_num_participations =
    Assignment.contact_participation_in_other_assignments
      database_label
      [ assignment ]
      experiment_id
      contact_id
    >|+ not
    ||> get_exn
  in
  (assignment, no_show, participated, increment_num_participations, None)
  |> CCList.pure
  |> SetAttendance.handle session
  |> get_exn
  |> Pool_event.handle_events database_label
;;

let delete_assignment experiment_id contact sessions =
  let open Utils.Lwt_result.Infix in
  let open Assignment_command in
  let%lwt decrement_num_participations =
    Assignment.(
      contact_participation_in_other_assignments
        database_label
        sessions
        experiment_id
        (Contact.id contact)
      >|+ not
      ||> get_exn)
  in
  (contact, sessions, decrement_num_participations)
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
    let%lwt expected =
      Lwt.return
        Contact.{ contact with num_assignments = NumberOfAssignments.of_int 2 }
    in
    let%lwt res = get_contact contact_id in
    let () = Alcotest.(check Test_utils.contact "succeeds" expected res) in
    let%lwt () = set_sessions_to_past [ session_id; followup_session_id ] in
    Lwt.return_unit
  ;;

  let close_first_session _ () =
    let%lwt contact = get_contact contact_id in
    let%lwt session = get_session session_id in
    let%lwt () = close_session session contact_id experiment_id in
    let contact =
      Contact.
        { contact with
          num_show_ups = NumberOfShowUps.of_int 1
        ; num_participations = NumberOfParticipations.of_int 1
        }
    in
    let%lwt res = get_contact contact_id in
    let () = Alcotest.(check Test_utils.contact "succeeds" contact res) in
    Lwt.return_unit
  ;;

  let close_follow_up_session _ () =
    let%lwt contact = get_contact contact_id in
    let%lwt follow_up = get_session followup_session_id in
    let%lwt () = close_session follow_up contact_id experiment_id in
    let%lwt res = get_contact contact_id in
    let contact =
      Contact.{ contact with num_show_ups = NumberOfShowUps.of_int 2 }
    in
    let () = Alcotest.(check Test_utils.contact "succeeds" contact res) in
    Lwt.return_unit
  ;;
end

module CancelSession = struct
  let contact_id = Contact.Id.create ()
  let session_id = Session.Id.create ()
  let followup_session_id = Session.Id.create ()
  let experiment_id = Experiment.Id.create ()
  let initialize = initialize contact_id experiment_id session_id

  let without_followups _ () =
    let open Utils.Lwt_result.Infix in
    let%lwt contact, experiment, session, _ = initialize () in
    let%lwt () = sign_up_for_session experiment contact session_id in
    let%lwt expected =
      Lwt.return
        Contact.{ contact with num_assignments = NumberOfAssignments.of_int 1 }
    in
    let%lwt res = get_contact contact_id in
    let () = Alcotest.(check Test_utils.contact "succeeds" expected res) in
    let%lwt () =
      let open Cqrs_command.Session_command.Cancel in
      let%lwt assignments =
        Assignment.find_uncanceled_by_session database_label session_id
        >|+ Assignment.group_by_contact
        ||> get_exn
      in
      let email = Model.create_email () in
      let reason = "Some reason" |> Session.CancellationReason.of_string in
      handle
        [ session ]
        assignments
        (fun _ _ -> Ok email)
        { reason; notify_email = true; notify_sms = false }
      |> get_exn
      |> Pool_event.handle_events database_label
    in
    let%lwt res = get_contact contact_id in
    let contact =
      Contact.{ contact with num_assignments = NumberOfAssignments.of_int 0 }
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
      Contact.(contact |> decrement_num_assignments |> decrement_num_show_ups)
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
        |> decrement_num_assignments
        |> decrement_num_show_ups
        |> decrement_num_participations)
    in
    let%lwt res = get_contact contact_id in
    let () = Alcotest.(check Test_utils.contact "succeeds" contact res) in
    Lwt.return_unit
  ;;
end
