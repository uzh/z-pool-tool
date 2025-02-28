open Utils.Lwt_result.Infix
open Authorization_test_utils
open Alcotest

let create_actor = Integration_utils.create_admin_actor

let query_item_count (_, { Query.pagination; _ }) =
  pagination
  |> CCOption.get_exn_or "Pagination not found"
  |> fun { Query.Pagination.item_count; _ } -> item_count
;;

let experiments _ () =
  let testable = list Test_utils.experiment in
  let sort = CCList.sort Experiment.compare in
  let%lwt actor = create_actor () in
  let%lwt all_experiments = Experiment.all pool ||> sort in
  let get_by_user () = Experiment.list_by_user pool actor ||> fst ||> sort in
  (* Without any roles *)
  let%lwt experiments = get_by_user () in
  check testable "No experiments returned" [] experiments;
  (* Assign assistant experiment 1 *)
  let first = CCList.hd all_experiments in
  let%lwt () = assign_role actor `Assistant (Some (experiment_target first)) in
  let%lwt experiments = get_by_user () in
  check testable "first experiment returned" [ first ] experiments;
  (* Assign experimenter experiment 2 *)
  let second = CCList.nth all_experiments 2 in
  let%lwt () = assign_role actor `Experimenter (Some (experiment_target second)) in
  let%lwt experiments = get_by_user () in
  check testable "first and second experiments returned" [ first; second ] experiments;
  (* Assign global assistant *)
  let%lwt () = assign_role actor `Assistant None in
  let%lwt experiments = get_by_user () in
  check testable "all experiments returned" all_experiments experiments;
  (* Revoke global assistant *)
  let%lwt () = revoke_role actor `Assistant None in
  let%lwt experiments = get_by_user () in
  check testable "first and second experiments returned" [ first; second ] experiments;
  (* Revoke experimenter experiment 2 *)
  let%lwt () = revoke_role actor `Experimenter (Some (experiment_target second)) in
  let%lwt experiments = get_by_user () in
  check testable "first experiment returned" [ first ] experiments;
  (* Assign experimenter experiment 1 (assistant still exists) *)
  let%lwt () = assign_role actor `Experimenter (Some (experiment_target first)) in
  let%lwt experiments = get_by_user () in
  check testable "first experiment returned" [ first ] experiments;
  Lwt.return_unit
;;

let locations _ () =
  let testable = list Test_utils.location in
  let sort = CCList.sort Pool_location.compare in
  let%lwt actor = create_actor () in
  let%lwt all_locations = Pool_location.all pool ||> sort in
  let get_by_user () = Pool_location.list_by_user pool actor ||> fst ||> sort in
  (* Without any roles *)
  let%lwt locations = get_by_user () in
  check testable "No locations returned" [] locations;
  (* Assign location_manager location 1 *)
  let first = CCList.hd all_locations in
  let%lwt () = assign_role actor `LocationManager (Some (location_target first)) in
  let%lwt locations = get_by_user () in
  check testable "first location returned" [ first ] locations;
  (* Assign location_manager location 2 *)
  let second = CCList.nth all_locations 2 in
  let%lwt () = assign_role actor `LocationManager (Some (location_target second)) in
  let%lwt locations = get_by_user () in
  check testable "first and second location returned" [ first; second ] locations;
  (* Assign location_manager globally *)
  let%lwt () = assign_role actor `LocationManager None in
  let%lwt locations = get_by_user () in
  check testable "all locations returned" all_locations locations;
  (* Revoke global location_manager *)
  let%lwt () = revoke_role actor `LocationManager None in
  let%lwt locations = get_by_user () in
  check testable "first and second location returned" [ first; second ] locations;
  Lwt.return_unit
;;

let contacts _ () =
  let open Integration_utils in
  let testable = list Test_utils.contact in
  let sort = CCList.sort Contact.compare in
  let%lwt actor = create_actor () in
  let get_by_user () = Contact.list_by_user pool actor ||> fst ||> sort in
  (* Without any roles *)
  let%lwt contacts = get_by_user () in
  check testable "No contacts returned" [] contacts;
  (* As assistant *)
  let%lwt exp1 = ExperimentRepo.create () in
  let%lwt () = assign_role actor `Assistant (Some (experiment_target exp1)) in
  let%lwt contacts = get_by_user () in
  check testable "No contacts returned" [] contacts;
  (* As assistant with participants *)
  let%lwt contact1 = ContactRepo.create () in
  let%lwt session1 = SessionRepo.create exp1 () in
  let%lwt (_ : Assignment.t) = AssignmentRepo.create session1 contact1 in
  let%lwt contacts = get_by_user () in
  check testable "Experiment participant returned" [ contact1 ] contacts;
  (* As location manager *)
  let%lwt contact2 = ContactRepo.create () in
  let%lwt location = LocationRepo.create () in
  let%lwt exp2 = ExperimentRepo.create () in
  let%lwt session2 = SessionRepo.create ~location exp2 () in
  let%lwt (_ : Assignment.t) = AssignmentRepo.create session2 contact2 in
  let%lwt () = assign_role actor `LocationManager (Some (location_target location)) in
  let%lwt contacts = get_by_user () in
  let expected = [ contact1; contact2 ] |> sort in
  check testable "Contacts 1, 2 returned" expected contacts;
  (* With actor permission *)
  let%lwt contact3 = ContactRepo.create () in
  let%lwt () =
    create_actor_permission actor Guard.Permission.Read (contact_target contact3)
  in
  let%lwt contacts = get_by_user () in
  let expected = [ contact1; contact2; contact3 ] |> sort in
  check testable "Contacts 1, 2, 3 returned" expected contacts;
  (* As recruiter *)
  let%lwt () = assign_role actor `Recruiter None in
  let query =
    let open Query in
    create ~pagination:(Pagination.create ~item_count:1 ()) ()
  in
  let%lwt expected = Contact.all ~query pool ||> query_item_count in
  let%lwt contact_count = Contact.list_by_user ~query pool actor ||> query_item_count in
  check int "All contacts are returned" expected contact_count;
  Lwt.return_unit
;;

let admins _ () =
  let testable = list Test_utils.admin in
  let sort = CCList.sort Admin.compare in
  let%lwt actor = create_actor () in
  let get_by_user () = Admin.list_by_user pool actor ||> fst ||> sort in
  (* Without any roles *)
  let%lwt admins = get_by_user () in
  check testable "No admins returned" [] admins;
  (* As assistant *)
  let%lwt () = assign_role actor `Assistant None in
  let%lwt admins = get_by_user () in
  check testable "No admins returned" [] admins;
  (* With actor permission *)
  let%lwt admin = Integration_utils.AdminRepo.create () in
  let%lwt () = create_actor_permission actor Guard.Permission.Read (admin_target admin) in
  let%lwt admins = get_by_user () in
  check testable "One admin returned" [ admin ] admins;
  (* As recruiter *)
  let%lwt () = assign_role actor `Recruiter None in
  let query =
    let open Query in
    create ~pagination:(Pagination.create ~item_count:1 ()) ()
  in
  let%lwt expected = Admin.all ~query pool ||> query_item_count in
  let%lwt admin_count = Admin.list_by_user ~query pool actor ||> query_item_count in
  check int "All admins are returned" expected admin_count;
  Lwt.return_unit
;;

module CalendarUtils = struct
  let actor_permissions actor =
    actor.Guard.Actor.uuid |> Guard.Persistence.ActorRole.permissions_of_actor pool
  ;;

  open Session

  let testable = list Calendar.(testable pp equal)
  let sort = CCList.sort Calendar.compare
  let session_ids = CCList.map (fun { id; _ } -> id)
  let calendar_session_ids = CCList.map (fun (t : Calendar.t) -> t.Calendar.id)
  let start_time = Ptime_clock.now ()

  let end_time =
    Ptime.add_span start_time Sihl.Time.(duration_to_span OneWeek)
    |> CCOption.get_exn_or "Invalid timespan"
  ;;

  let start = Test_utils.Model.in_an_hour ()

  let make_links
        ?(experiment_link = true)
        ?(session_link = true)
        ?(location_link = true)
        session
    =
    let open Http_utils.Url.Admin in
    let experiment_id = session.experiment.Experiment.id in
    let location_id = session.location.Pool_location.id in
    let experiment_link =
      if experiment_link
      then Some (experiment_path ~id:experiment_id () |> Sihl.Web.externalize_path)
      else None
    in
    let session_link =
      match session_link, location_link with
      | true, _ ->
        Some (session_path experiment_id ~id:session.id |> Sihl.Web.externalize_path)
      | _, true ->
        Some (location_session_path location_id session.id () |> Sihl.Web.externalize_path)
      | false, false -> None
    in
    Calendar.{ experiment = experiment_link; session = session_link }
  ;;

  let to_calendar
        ?(experiment_link = true)
        ?(session_link = true)
        ?(location_link = true)
        ({ experiment; _ } as session)
    =
    let open Calendar in
    let open Experiment in
    let location =
      Pool_location.{ id = session.location.id; name = session.location.name }
    in
    let links = make_links ~experiment_link ~session_link ~location_link session in
    { id = session.id
    ; experiment_id = experiment.id
    ; title = experiment.title
    ; contact_email = experiment.contact_email
    ; start = session.start
    ; end_ =
        Duration.value session.duration
        |> End.build session.start
        |> Test_utils.get_or_failwith
    ; links
    ; max_participants = session.max_participants
    ; min_participants = session.min_participants
    ; overbook = session.overbook
    ; assignment_count = session.assignment_count
    ; internal_description = session.internal_description
    ; location
    }
  ;;

  let calendar_by_user actor =
    actor_permissions actor
    >|> Session.calendar_by_user ~start_time ~end_time pool actor
    ||> sort
  ;;

  let calendar_by_location { Pool_location.id; _ } actor =
    actor_permissions actor
    >|> calendar_by_location ~location_uuid:id ~start_time ~end_time pool actor
    ||> sort
  ;;
end

let dashboard_calendar _ () =
  let open Integration_utils in
  let open CalendarUtils in
  let%lwt actor = create_actor () in
  let get_by_user () = calendar_by_user actor in
  (* Without any roles *)
  let%lwt sessions = get_by_user () in
  check testable "No sessions returned" [] sessions;
  (* With session specific role *)
  let%lwt exp1 = ExperimentRepo.create () in
  let%lwt session1 =
    SessionRepo.create ~start exp1 () ||> to_calendar ~experiment_link:false
  in
  let%lwt () = assign_role actor `Assistant (Some (calendar_target session1)) in
  let%lwt sessions = get_by_user () in
  check testable "session1 returned" [ session1 ] sessions;
  (* With experiment specific role *)
  let%lwt exp2 = ExperimentRepo.create () in
  let%lwt session2 = SessionRepo.create ~start exp2 () ||> to_calendar in
  let%lwt () = assign_role actor `Assistant (Some (experiment_target exp2)) in
  let%lwt sessions = get_by_user () in
  let expected = [ session1; session2 ] |> sort in
  check testable "session1 & session2 returned" expected sessions;
  (* As location manager *)
  let%lwt location = LocationRepo.create () in
  let%lwt exp3 = ExperimentRepo.create () in
  let%lwt session3 =
    SessionRepo.create ~start ~location exp3 ()
    ||> to_calendar ~experiment_link:false ~session_link:false
  in
  let%lwt () = assign_role actor `LocationManager (Some (location_target location)) in
  let%lwt sessions = get_by_user () in
  let expected = [ session1; session2; session3 ] |> sort in
  check testable "session 1 - 3 returned" expected sessions;
  Lwt.return_unit
;;

let location_calendar _ () =
  let open Integration_utils in
  let open CalendarUtils in
  let%lwt actor = create_actor () in
  let%lwt location = LocationRepo.create () in
  let get_by_user () = calendar_by_location location actor in
  (* Without any roles *)
  let%lwt sessions = get_by_user () in
  check testable "No sessions returned" [] sessions;
  (* As location manager *)
  let%lwt () = assign_role actor `LocationManager (Some (location_target location)) in
  let%lwt exp = ExperimentRepo.create () in
  let%lwt session1 = SessionRepo.create ~start ~location exp () in
  let%lwt sessions = get_by_user () in
  let expected = [ to_calendar ~experiment_link:false ~session_link:false session1 ] in
  check testable "session1 returned" expected sessions;
  (* With additional session specific access, at another location *)
  let%lwt session2 = SessionRepo.create ~start exp () in
  let%lwt () = assign_role actor `Assistant (Some (session_target session2)) in
  check testable "session1 returned" expected sessions;
  Lwt.return_unit
;;
