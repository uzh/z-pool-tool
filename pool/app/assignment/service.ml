open Entity

let src = Logs.Src.create "assignment.service"
let get_or_failwith = Pool_common.Utils.get_or_failwith

let update_matches_filter database_label =
  let open Utils.Lwt_result.Infix in
  let open Repo in
  let handle filter =
    Lwt_list.map_s (fun ({ contact; _ } as assignment) ->
      match filter with
      | None -> Lwt.return (assignment, MatchesFilter.create true)
      | Some filter ->
        Filter.contact_matches_filter database_label filter.Filter.query contact
        ||> MatchesFilter.create
        ||> CCPair.make assignment)
  in
  function
  | `Session ({ Session.id; _ }, filter) ->
    find_all_by_session database_label id >|> handle filter ||> CCResult.return
  | `Experiment ({ Experiment.id; _ }, filter) ->
    Sql.find_all_by_experiment database_label id
    >|> handle filter
    ||> CCResult.return
  | `Upcoming ->
    Sql.find_all_upcoming database_label
    |>> Lwt_list.fold_left_s
          (fun acc (experiment, assignments) ->
            handle experiment.Experiment.filter assignments
            ||> CCList.append acc)
          []
;;

let update_matches_filter_events =
  CCList.filter_map (fun (assignment, matches_filter) ->
    match MatchesFilter.equal assignment.matches_filter matches_filter with
    | true -> None
    | false -> Some ({ assignment with matches_filter } |> Event.updated))
;;

let update_upcoming_assignments database_label =
  let open Utils.Lwt_result.Infix in
  update_matches_filter database_label `Upcoming
  >|+ update_matches_filter_events
  |>> Lwt_list.iter_s (Event.handle_event database_label)
  ||> get_or_failwith
;;

let start () =
  let open Schedule in
  let interval = Ptime.Span.of_int_s (60 * 60) |> ScheduledTimeSpan.of_span in
  let periodic_fcn () =
    Logs.debug ~src (fun m ->
      m
        ~tags:Pool_database.(Logger.Tags.create root)
        "Run check if future assignments match filter");
    Lwt.bind
      (Pool_tenant.find_databases ())
      (Lwt_list.iter_s (fun { Pool_database.label; _ } ->
         update_upcoming_assignments label))
  in
  create "upcoming_assignments_match_filter" (Every interval) periodic_fcn
  |> Schedule.add_and_start
;;

let stop () = Lwt.return_unit

let lifecycle =
  Sihl.Container.create_lifecycle
    "System events"
    ~dependencies:(fun () -> [ Database.lifecycle ])
    ~start
    ~stop
;;

let register () = Sihl.Container.Service.create lifecycle
