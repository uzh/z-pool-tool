let day, hour, halfhour =
  let map f (a, b, c) = f a, f b, f c in
  (24 * 60 * 60, 60 * 60, 30 * 60) |> map Ptime.Span.of_int_s
;;

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
  let from_now_ms offset =
    Ptime.add_span (Ptime_clock.now ()) offset |> CCOption.get_exn_or "Invalid time"
  in
  let time_window_data =
    [ ( "Morning session - 1"
      , "Initial data collection phase"
      , Some 10
      , Ptime_clock.now ()
      , hour )
    ; ( "Afternoon session - 1"
      , "Follow-up data collection"
      , Some 10
      , Ptime_clock.now ()
      , hour )
    ; ( "Evening session"
      , "Extended availability for remote participants"
      , Some 25
      , from_now_ms day
      , day )
    ]
  in
  let create_timewindow
        experiment
        (public_description, internal_description, max_participants, start, duration)
    =
    let start = start |> Session.Start.create in
    let duration = duration |> Session.Duration.create |> get_or_failwith in
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
      time_window_data |> CCList.map (create_timewindow experiment))
    |> CCList.map (fun tw -> Time_window.created tw)
  in
  let%lwt () = Lwt_list.iter_s (Time_window.handle_event pool) events in
  Lwt.return_unit
;;
