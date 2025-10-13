let timewindow pool =
  let open Utils.Lwt_result.Infix in
  let%lwt online_experiments =
    let open Experiment in
    let all_experiments = Experiment.all pool in
    let is_online_experiment { online_experiment; _ } =
      online_experiment |> CCOption.is_some
    in
    all_experiments ||> CCList.filter is_online_experiment
  in
  let get_or_failwith = Pool_common.Utils.get_or_failwith in
  let parse_time str =
    str |> Ptime.of_rfc3339 |> CCResult.get_exn |> fun (t, _, _) -> t
  in
  let time_window_data =
    [ ( "Morning session - Week 1"
      , "Initial data collection phase"
      , Some 10
      , "2025-10-15T09:00:00Z"
      , "2025-10-15T12:00:00Z" )
    ; ( "Afternoon session - Week 1"
      , "Follow-up data collection"
      , Some 10
      , "2025-10-15T14:00:00Z"
      , "2025-10-15T17:00:00Z" )
    ; ( "Evening session - Weekend"
      , "Extended availability for remote participants"
      , Some 25
      , "2025-10-19T18:00:00Z"
      , "2025-10-19T22:00:00Z" )
    ]
  in
  let create_timewindow
        (public_description, internal_description, max_participants, start_str, end_str)
        experiment
    =
    let start = parse_time start_str |> Session.Start.create in
    let end_at = parse_time end_str in
    let start_time = Session.Start.value start in
    let duration =
      Ptime.diff end_at start_time |> Session.Duration.create |> get_or_failwith
    in
    let public_description =
      Session.PublicDescription.create public_description |> get_or_failwith
    in
    let internal_description =
      Session.InternalDescription.create internal_description |> get_or_failwith
    in
    let max_participants =
      max_participants
      |> CCOption.map (fun n -> Session.ParticipantAmount.create n |> get_or_failwith)
    in
    Time_window.create
      ~public_description
      ~internal_description
      ?max_participants
      start
      duration
      experiment
  in
  let events =
    online_experiments
    |> CCList.flat_map (fun experiment ->
      time_window_data
      |> CCList.map (fun time_window -> create_timewindow time_window experiment))
    |> CCList.map (fun tw -> Time_window.Created tw)
  in
  let%lwt () = Lwt_list.iter_s (Time_window.handle_event pool) events in
  Lwt.return_unit
;;
