open Test_utils
open Cqrs_command
open Utils.Lwt_result.Infix
open Pool_message

let database_label = Data.database_label
let current_user () = Integration_utils.AdminRepo.create () |> Lwt.map Pool_context.admin
let get_exn = get_or_failwith
let get_contact contact_id = contact_id |> Contact.find database_label |> Lwt.map get_exn
let get_session session_id = session_id |> Session.find database_label |> Lwt.map get_exn

let get_experiment experiment_id =
  experiment_id |> Experiment.find database_label |> Lwt.map get_exn
;;

let confirmation_mail (_ : Assignment.t) =
  Common_test.Data.create_email () |> Email.Service.Job.create |> Email.create_dispatch
;;

let invitation_mail (_ : Contact.t) =
  Common_test.Data.create_email ()
  |> Email.Service.Job.create
  |> Email.create_dispatch
  |> CCResult.return
;;

let find_assignment_by_contact_and_session contact_id session_id =
  let open Assignment in
  find_uncanceled_by_session database_label session_id
  ||> CCList.find (fun ({ contact; _ } : Assignment.t) ->
    Contact.(Id.equal (id contact) contact_id))
;;

let set_sessions_to_past session_ids =
  let open Session in
  let%lwt current_user = current_user () in
  session_ids
  |> Lwt_list.map_s (fun id -> find database_label id)
  ||> CCResult.flatten_l
  >|+ CCList.map (fun (session : t) ->
    let updated = { session with start = Model.an_hour_ago () } in
    Updated (session, updated) |> Pool_event.session)
  |>> Pool_event.handle_events database_label current_user
  ||> get_exn
;;

let sign_up_for_session experiment contact session_id =
  let%lwt current_user = current_user () in
  let%lwt session = Session.find_open database_label session_id ||> get_exn in
  let%lwt follow_up_sessions = Session.find_follow_ups database_label session_id in
  Assignment_command.Create.(handle { contact; session; follow_up_sessions; experiment })
    confirmation_mail
    false
  |> get_exn
  |> Pool_event.handle_events database_label current_user
;;

let close_session ?(no_show = false) ?(participated = true) session contact_id experiment =
  let open Assignment in
  let open Session_command in
  let%lwt current_user = current_user () in
  let%lwt assignment =
    find_assignment_by_contact_and_session contact_id session.Session.id
  in
  let assignment =
    { assignment with
      no_show = Some (NoShow.create no_show)
    ; participated = Some (Participated.create participated)
    }
  in
  let%lwt increment_num_participations =
    contact_participation_in_other_assignments
      database_label
      ~exclude_assignments:[ assignment ]
      experiment.Experiment.id
      contact_id
    >|+ not
    >|+ IncrementParticipationCount.create
    ||> get_exn
  in
  (assignment, increment_num_participations, None)
  |> CCList.pure
  |> Close.handle experiment session []
  |> get_exn
  |> Pool_event.handle_events database_label current_user
;;

let delete_assignment experiment_id contact assignments =
  let open Assignment_command in
  let%lwt current_user = current_user () in
  let%lwt decrement_num_participations =
    Assignment.(
      contact_participation_in_other_assignments
        database_label
        ~exclude_assignments:assignments
        experiment_id
        (Contact.id contact)
      >|+ not
      >|+ IncrementParticipationCount.create
      ||> get_exn)
  in
  (contact, assignments, decrement_num_participations)
  |> MarkAsDeleted.handle
  |> get_exn
  |> Pool_event.handle_events database_label current_user
;;

let initialize contact_id experiment_id session_id ?followup_session_id () =
  let open Integration_utils in
  let%lwt admin = current_user () in
  let%lwt contact = ContactRepo.create ~id:contact_id ~with_terms_accepted:true () in
  let%lwt experiment = ExperimentRepo.create ~id:experiment_id () in
  let%lwt session = SessionRepo.create ~id:session_id experiment () in
  let%lwt follow_up_session =
    followup_session_id
    |> CCOption.map_or ~default:Lwt.return_none (fun id ->
      SessionRepo.create ~id ~follow_up_to:session_id experiment ()
      |> Lwt.map CCOption.return)
  in
  Lwt.return (contact, experiment, session, follow_up_session, admin)
;;

let initialize_online_survey contact_id experiment_id time_window_id () =
  let open Integration_utils in
  let open Test_utils in
  let%lwt contact = ContactRepo.create ~id:contact_id ~with_terms_accepted:true () in
  let%lwt experiment =
    ExperimentRepo.create
      ~id:experiment_id
      ~online_experiment:Experiment_test.Data.online_experiment
      ()
  in
  let%lwt time_window =
    TimeWindowRepo.create
      ~id:time_window_id
      (Model.in_an_hour ())
      (Session.Duration.create Model.hour |> get_exn)
      experiment
      ()
  in
  Lwt.return (contact, experiment, time_window)
;;

module InviteContact = struct
  let contact_id = Contact.Id.create ()
  let experiment_id = Experiment.Id.create ()
  let session_id = Session.Id.create ()
  let initialize = initialize contact_id experiment_id session_id

  let invite _ () =
    let%lwt contact, experiment, _, _, current_user = initialize () in
    let%lwt () =
      Invitation_command.Create.(
        handle
          { experiment
          ; mailing = None
          ; contacts = [ contact ]
          ; invited_contacts = []
          ; create_message = invitation_mail
          })
      |> get_exn
      |> Pool_event.handle_events database_label current_user
    in
    let%lwt res = get_contact contact_id in
    let expected = contact |> Contact.update_num_invitations ~step:1 in
    let () =
      Alcotest.(check Test_utils.contact "num invitations increased" expected res)
    in
    Lwt.return ()
  ;;
end

module AttendAll = struct
  let contact_id = Contact.Id.create ()
  let session_id = Session.Id.create ()
  let followup_session_id = Session.Id.create ()
  let experiment_id = Experiment.Id.create ()

  let experiment () =
    Experiment.find Test_utils.Data.database_label experiment_id ||> get_exn
  ;;

  let initialize = initialize contact_id experiment_id session_id ~followup_session_id

  let register_for_session _ () =
    let%lwt contact, experiment, _, _, _ = initialize () in
    let%lwt () = sign_up_for_session experiment contact session_id in
    let%lwt res = get_contact contact_id in
    let%lwt expected =
      Lwt.return Contact.{ res with num_assignments = NumberOfAssignments.of_int 2 }
    in
    let () = Alcotest.(check Test_utils.contact "succeeds" expected res) in
    let%lwt () = set_sessions_to_past [ session_id; followup_session_id ] in
    Lwt.return_unit
  ;;

  let close_first_session _ () =
    let%lwt session = get_session session_id in
    let%lwt experiment = experiment () in
    let%lwt () = close_session session contact_id experiment in
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
    let%lwt experiment = experiment () in
    let%lwt () = close_session follow_up contact_id experiment in
    let%lwt res = get_contact contact_id in
    let contact = Contact.{ res with num_show_ups = NumberOfShowUps.of_int 2 } in
    let () = Alcotest.(check Test_utils.contact "succeeds" contact res) in
    Lwt.return_unit
  ;;
end

module CancelSession = struct
  let initialize ?followup_session_id () =
    initialize
      ?followup_session_id
      (Contact.Id.create ())
      (Experiment.Id.create ())
      (Session.Id.create ())
      ()
  ;;

  let test_cancellation contact_id initial_nr_assignments test_cases =
    let%lwt contact = get_contact contact_id in
    let%lwt current_user = current_user () in
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
        let%lwt follow_ups = Session.find_follow_ups database_label session.Session.id in
        let%lwt assignments =
          session :: follow_ups
          |> Lwt_list.fold_left_s
               (fun assignments session ->
                  Assignment.find_uncanceled_by_session database_label session.Session.id
                  ||> CCList.append assignments)
               []
          ||> Assignment.group_by_contact
        in
        let email = Model.create_email () in
        let reason = "Some reason" |> Session.CancellationReason.of_string in
        handle
          (session :: follow_ups)
          assignments
          (fun _ _ -> Ok (Email.Service.Job.create email |> Email.create_dispatch))
          Session_test.create_cancellation_text_message
          [ Pool_common.NotifyVia.Email ]
          reason
        |> get_exn
        |> Pool_event.handle_events database_label current_user
      in
      test_result expected_nr_assignments)
  ;;

  let without_followups _ () =
    let%lwt contact, experiment, session, _, _ = initialize () in
    let%lwt () = sign_up_for_session experiment contact session.Session.id in
    test_cancellation (Contact.id contact) 1 [ session, 0 ]
  ;;

  let follow_up _ () =
    let%lwt contact, experiment, session, followup_session, _ =
      initialize ~followup_session_id:(Session.Id.create ()) ()
    in
    let followup_session =
      followup_session |> CCOption.get_exn_or "Follow up session is required"
    in
    let%lwt () = sign_up_for_session experiment contact session.Session.id in
    test_cancellation (Contact.id contact) 2 [ followup_session, 1; session, 0 ]
  ;;

  let main_with_follow_up _ () =
    let%lwt contact, experiment, session, _, _ =
      initialize ~followup_session_id:(Session.Id.create ()) ()
    in
    let%lwt () = sign_up_for_session experiment contact session.Session.id in
    test_cancellation (Contact.id contact) 2 [ session, 0 ]
  ;;
end

module DoNotAttend = struct
  let contact_id = Contact.Id.create ()
  let session_id = Session.Id.create ()
  let experiment_id = Experiment.Id.create ()

  let experiment () =
    Experiment.find Test_utils.Data.database_label experiment_id ||> get_exn
  ;;

  let initialize = initialize contact_id experiment_id session_id

  let register_for_session _ () =
    let%lwt contact, experiment, _, _, _ = initialize () in
    let%lwt () = sign_up_for_session experiment contact session_id in
    let%lwt res = get_contact contact_id in
    let%lwt expected =
      Lwt.return Contact.{ res with num_assignments = NumberOfAssignments.of_int 1 }
    in
    let () = Alcotest.(check Test_utils.contact "succeeds" expected res) in
    let%lwt () = set_sessions_to_past [ session_id ] in
    Lwt.return_unit
  ;;

  let close_main _ () =
    let%lwt session = get_session session_id in
    let%lwt experiment = experiment () in
    let%lwt () = close_session ~participated:false session contact_id experiment in
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

  let experiment () =
    Experiment.find Test_utils.Data.database_label experiment_id ||> get_exn
  ;;

  let initialize = initialize contact_id experiment_id session_id

  let register_for_session _ () =
    let%lwt contact, experiment, _, _, _ = initialize () in
    let%lwt () = sign_up_for_session experiment contact session_id in
    let%lwt res = get_contact contact_id in
    let%lwt expected =
      Lwt.return Contact.{ res with num_assignments = NumberOfAssignments.of_int 1 }
    in
    let () = Alcotest.(check Test_utils.contact "succeeds" expected res) in
    let%lwt () = set_sessions_to_past [ session_id ] in
    Lwt.return_unit
  ;;

  let close_main _ () =
    let%lwt experiment = experiment () in
    let%lwt session = get_session session_id in
    let%lwt () =
      close_session ~no_show:true ~participated:false session contact_id experiment
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
        contact |> update_num_assignments ~step:(-1) |> update_num_show_ups ~step:(-1))
    in
    let%lwt res = get_contact contact_id in
    let () = Alcotest.(check Test_utils.contact "succeeds" contact res) in
    Lwt.return_unit
  ;;

  let delete_main _ () =
    let%lwt session = find_assignment_by_contact_and_session contact_id session_id in
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
  let initialize = initialize contact_id experiment_id session_id ~followup_session_id
  let num_assignments = 4

  let register_for_session _ () =
    let%lwt contact, experiment, _, _, current_user = initialize () in
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
      |> Pool_event.handle_event database_label current_user
    in
    let%lwt () = sign_up_for_session experiment contact session_id in
    let%lwt res = get_contact contact_id in
    let%lwt expected =
      Lwt.return
        Contact.
          { res with num_assignments = NumberOfAssignments.of_int (num_assignments + 2) }
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

module UpdateAssignments = struct
  open Cqrs_command.Assignment_command
  open CCResult
  open Contact

  let contact_id = Contact.Id.create ()
  let session_id = Session.Id.create ()
  let followup_session_id = Session.Id.create ()
  let experiment_id = Experiment.Id.create ()
  let initial_assignments = NumberOfAssignments.of_int 2
  let initial_showups = NumberOfShowUps.of_int 1
  let initial_noshows = NumberOfNoShows.of_int 1
  let initial_participations = NumberOfParticipations.of_int 1

  let initialize () =
    initialize contact_id experiment_id session_id ~followup_session_id ()
    >|> fun (contact, session, experiment, follow_ups, _) ->
    let contact =
      { contact with
        num_assignments = initial_assignments
      ; num_show_ups = initial_showups
      ; num_no_shows = initial_noshows
      ; num_participations = initial_participations
      }
    in
    let%lwt () = Updated contact |> handle_event database_label in
    Lwt.return (contact, session, experiment, follow_ups)
  ;;

  let get_entities () =
    let%lwt contact = get_contact contact_id in
    let%lwt session = get_session session_id in
    let%lwt experiment = get_experiment experiment_id in
    let%lwt followup_session = get_session followup_session_id in
    Lwt.return (contact, experiment, session, followup_session)
  ;;

  let to_urlencoded ?external_data_id ~no_show ~participated () =
    let bool_to_string = Pool_model.Base.Boolean.stringify in
    let base =
      [ Field.(show NoShow), [ bool_to_string no_show ]
      ; Field.(show Participated), [ bool_to_string participated ]
      ]
    in
    match external_data_id with
    | None -> base
    | Some id -> base @ [ Field.(show ExternalDataId), [ id ] ]
  ;;

  let update_unclosed _ () =
    let open Update in
    let%lwt contact, experiment, session, _ = initialize () in
    let%lwt () = sign_up_for_session experiment contact session_id in
    let%lwt assignment = find_assignment_by_contact_and_session contact_id session_id in
    let participated_in_other_sessions = false in
    let res =
      to_urlencoded ~no_show:true ~participated:false ()
      |> decode
      >>= handle experiment (`Session session) assignment participated_in_other_sessions
    in
    let expected =
      Assignment.(
        Updated
          ( assignment
          , { assignment with
              no_show = Some (NoShow.create true)
            ; participated = Some (Participated.create false)
            } ))
      |> Pool_event.assignment
      |> CCList.return
      |> CCResult.return
    in
    let () =
      check_result ~msg:"Cannot update assignment of unclosed session" expected res
    in
    Lwt.return_unit
  ;;

  let close_main_session _ () =
    let%lwt contact, experiment, session, _ = get_entities () in
    let%lwt () =
      close_session ~no_show:false ~participated:true session contact_id experiment
    in
    let%lwt updated_contact = get_contact contact_id in
    let expected =
      contact |> update_num_show_ups ~step:1 |> update_num_participations ~step:1
    in
    Alcotest.(
      check
        Test_utils.contact
        "Session close: counters were updated"
        expected
        updated_contact)
    |> Lwt.return
  ;;

  let update_assignment_manually _ () =
    let%lwt current_user = current_user () in
    let%lwt contact, experiment, session, _ = get_entities () in
    let participated_in_other_sessions assignments =
      Assignment.(
        contact_participation_in_other_assignments
          database_label
          ~exclude_assignments:assignments
          experiment_id
          contact_id
        ||> get_exn)
    in
    let handle_update assignment urlencoded =
      let%lwt participated_in_other_sessions =
        participated_in_other_sessions [ assignment ]
      in
      let open Update in
      urlencoded
      |> decode
      >>= handle experiment (`Session session) assignment participated_in_other_sessions
      |> get_exn
      |> Pool_event.handle_events database_label current_user
    in
    let%lwt () =
      let%lwt assignment = find_assignment_by_contact_and_session contact_id session_id in
      let%lwt () =
        to_urlencoded ~no_show:true ~participated:false () |> handle_update assignment
      in
      let expected =
        assignment.Assignment.contact
        |> update_num_show_ups ~step:(-1)
        |> update_num_no_shows ~step:1
        |> update_num_participations ~step:(-1)
      in
      let%lwt res = get_contact contact_id in
      Alcotest.(check Test_utils.contact "counters were manually updated" expected res)
      |> Lwt.return
    in
    let%lwt () =
      let%lwt assignment = find_assignment_by_contact_and_session contact_id session_id in
      let%lwt () =
        to_urlencoded ~no_show:false ~participated:true () |> handle_update assignment
      in
      let expected = contact in
      let%lwt res = get_contact contact_id in
      Alcotest.(check Test_utils.contact "counters were manually updated" expected res)
      |> Lwt.return
    in
    Lwt.return_unit
  ;;

  let update_online_assignment _ () =
    let open Cqrs_command.Assignment_command in
    let open Assignment in
    let contact_id = Contact.Id.create () in
    let experiment_id = Experiment.Id.create () in
    let timewindow_id = Session.Id.create () in
    let%lwt contact, experiment, time_window =
      initialize_online_survey contact_id experiment_id timewindow_id ()
    in
    let contact =
      { contact with
        num_assignments = initial_assignments
      ; num_show_ups = initial_showups
      ; num_no_shows = initial_noshows
      ; num_participations = initial_participations
      }
    in
    let assignment = create contact in
    let handle assignment =
      Update.handle experiment (`TimeWindow time_window) assignment false
    in
    let update ~no_show ~participated =
      { external_data_id = None
      ; no_show = NoShow.create no_show
      ; participated = Participated.create participated
      }
    in
    let () =
      let update = update ~no_show:false ~participated:true in
      let expected =
        Ok
          [ Updated
              ( assignment
              , { assignment with
                  no_show = Some update.no_show
                ; participated = Some update.participated
                } )
            |> Pool_event.assignment
          ]
      in
      check_result
        ~msg:"Update unsubmitted online assignment"
        expected
        (handle assignment update)
    in
    let assignment =
      { assignment with
        no_show = Some (NoShow.create false)
      ; participated = Some (Participated.create true)
      }
    in
    let () =
      let update = update ~no_show:false ~participated:true in
      let expected =
        Ok
          [ Updated
              ( assignment
              , { assignment with
                  no_show = Some update.no_show
                ; participated = Some update.participated
                } )
            |> Pool_event.assignment
          ; Contact.Updated contact |> Pool_event.contact
          ]
      in
      check_result
        ~msg:"Update submitted online assignment"
        expected
        (handle assignment update)
    in
    let () =
      let update = update ~no_show:true ~participated:false in
      let expected =
        Ok
          [ Updated
              ( assignment
              , { assignment with
                  no_show = Some update.no_show
                ; participated = Some update.participated
                } )
            |> Pool_event.assignment
          ; Contact.Updated
              (contact
               |> update_num_show_ups ~step:(-1)
               |> update_num_no_shows ~step:1
               |> update_num_participations ~step:(-1))
            |> Pool_event.contact
          ]
      in
      check_result
        ~msg:"Update submitted online assignment with no_show"
        expected
        (handle assignment update)
    in
    Lwt.return_unit
  ;;

  let close_followup_session _ () =
    let%lwt contact, experiment, _, followup_session = get_entities () in
    let%lwt () =
      close_session
        ~no_show:false
        ~participated:true
        followup_session
        contact_id
        experiment
    in
    let%lwt updated_contact = get_contact contact_id in
    let expected = contact |> update_num_show_ups ~step:1 in
    Alcotest.(
      check
        Test_utils.contact
        "Follow up session closed: counters were updated"
        expected
        updated_contact)
    |> Lwt.return
  ;;

  let update_follow_up_assignment_manually _ () =
    let%lwt current_user = current_user () in
    let%lwt _, experiment, _, followup_session = get_entities () in
    let participated_in_other_sessions assignments =
      Assignment.(
        contact_participation_in_other_assignments
          database_label
          ~exclude_assignments:assignments
          experiment_id
          contact_id
        ||> get_exn)
    in
    let handle_update assignment urlencoded =
      let%lwt participated_in_other_sessions =
        participated_in_other_sessions [ assignment ]
      in
      let open Update in
      urlencoded
      |> decode
      >>= handle
            experiment
            (`Session followup_session)
            assignment
            participated_in_other_sessions
      |> get_exn
      |> Pool_event.handle_events database_label current_user
    in
    let%lwt assignment =
      find_assignment_by_contact_and_session contact_id followup_session_id
    in
    let%lwt () =
      to_urlencoded ~no_show:true ~participated:false () |> handle_update assignment
    in
    let expected =
      assignment.Assignment.contact
      |> update_num_show_ups ~step:(-1)
      |> update_num_no_shows ~step:1
    in
    let%lwt res = get_contact contact_id in
    Alcotest.(check Test_utils.contact "counters were manually updated" expected res)
    |> Lwt.return
  ;;
end
