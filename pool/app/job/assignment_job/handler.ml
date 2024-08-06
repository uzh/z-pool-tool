module Notification = Message_template.MatchFilterUpdateNotification
open CCFun.Infix
open Utils.Lwt_result.Infix

let src = Logs.Src.create "job.assignment_job"
let get_or_failwith = Pool_common.Utils.get_or_failwith
let admins_to_notify = Experiment.find_admins_to_notify_about_invitations

let trigger_text =
  let open Pool_common.I18n in
  function
  | `Session _ -> MatchesFilterChangeReasonManually
  | `Upcoming -> MatchesFilterChangeReasonWorker
;;

let make_messages
  ?current_user
  context
  database_label
  ({ Experiment.id; _ } as experiment)
  sessions
  =
  let* tenant = Pool_tenant.find_by_label database_label in
  let make_mail assignments admin =
    let trigger = trigger_text context in
    Notification.create tenant trigger admin experiment assignments
  in
  let remove_matching =
    let open Assignment in
    let open CCList in
    filter_map (fun (session, assignments) ->
      assignments
      |> filter (fun { matches_filter; _ } ->
        MatchesFilter.value matches_filter |> not)
      |> function
      | [] -> None
      | assignments -> Some (session, assignments))
  in
  match remove_matching sessions with
  | [] -> Lwt_result.return []
  | sessions ->
    let%lwt admins =
      current_user
      |> CCOption.map_or
           ~default:(admins_to_notify database_label id)
           (CCList.return %> Lwt.return)
    in
    admins |> Lwt_list.map_s (make_mail sessions) |> Lwt_result.ok
;;

let make_events ?current_user context database_label experiment sessions =
  let open CCList in
  let open Utils.Lwt_result.Infix in
  let open Assignment in
  let filter_and_apply (assignment, matches_filter) =
    match MatchesFilter.equal assignment.matches_filter matches_filter with
    | true -> None
    | false -> Some { assignment with matches_filter }
  in
  let sessions =
    sessions
    |> filter_map (fun (session, assignments) ->
      assignments
      |> filter_map filter_and_apply
      |> function
      | [] -> None
      | assignments -> Some (session, assignments))
  in
  let* messages =
    make_messages context ?current_user database_label experiment sessions
  in
  let assignments =
    sessions
    |> flat_map snd
    >|= fun assignment -> assignment |> Assignment.updated
  in
  Lwt_result.return (assignments, messages)
;;

let handle_update ?current_user database_label context =
  let open Assignment in
  let handle experiment filter assignments =
    let matches_filter query contact =
      Filter.contact_matches_filter database_label query contact
      ||> MatchesFilter.create
    in
    assignments
    |> Lwt_list.map_s (fun (session, assignments) ->
      let%lwt assignments =
        assignments
        |> Lwt_list.map_s (fun ({ contact; _ } as assignment) ->
          match filter with
          | None -> Lwt.return (assignment, MatchesFilter.create true)
          | Some { Filter.query; _ } ->
            matches_filter query contact ||> CCPair.make assignment)
      in
      Lwt.return (session, assignments))
    >|> make_events ?current_user context database_label experiment
  in
  match context with
  | `Session ({ Session.id; experiment; _ } as session) ->
    let%lwt assignments = find_all_by_session database_label id in
    handle experiment experiment.Experiment.filter [ session, assignments ]
  | `Upcoming ->
    find_upcoming database_label
    >|> Lwt_list.fold_left_s
          (fun acc (experiment, assignments) ->
            let filter = Experiment.filter experiment in
            match acc with
            | Error err -> Lwt_result.fail err
            | Ok acc ->
              handle experiment filter assignments
              >|+ fun (assignments, emails) ->
              fst acc @ assignments, snd acc @ emails)
          (Ok ([], []))
;;

let update_upcoming_assignments database_label =
  handle_update database_label `Upcoming
  ||> get_or_failwith
  >|> fun (assignment_events, emails) ->
  let%lwt () =
    assignment_events
    |> Lwt_list.iter_s (Assignment.handle_event database_label)
  in
  Email.handle_event database_label (Email.BulkSent emails)
;;

let update_matches_filter
  ?current_user
  pool
  (context :
    [< `Experiment of Experiment.t * Filter.t option | `Session of Session.t ])
  =
  handle_update ?current_user pool context
;;
