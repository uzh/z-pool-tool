let create pool =
  let%lwt experiments = Experiment.find_all pool () in
  let%lwt location = Pool_location.find_all pool in
  let location =
    location |> CCList.head_opt |> CCOption.get_exn_or "No locations seeded"
  in
  let ex =
    experiments
    |> CCList.head_opt
    |> CCOption.get_exn_or "No experiments seeded"
  in
  let halfhour, hour = CCPair.map_same Ptime.Span.of_int_s (30 * 60, 60 * 60) in
  let data =
    [ ( ex.Experiment.id |> Pool_common.Id.value
      , Ptime.add_span (Ptime_clock.now ()) hour
        |> CCOption.get_exn_or "Invalid time"
      , hour
      , None
      , 30
      , 4
      , 4 )
    ; ( ex.Experiment.id |> Pool_common.Id.value
      , Ptime.add_span (Ptime_clock.now ()) hour
        |> CCOption.get_exn_or "Invalid time"
      , halfhour
      , Some "No metal allowed!"
      , 28
      , 20
      , 0 )
    ; ( ex.Experiment.id |> Pool_common.Id.value
      , Ptime.add_span (Ptime_clock.now ()) hour
        |> CCOption.get_exn_or "Invalid time"
      , halfhour
      , Some "No metal allowed!"
      , 30
      , 2
      , 5 )
    ]
  in
  let events =
    CCList.map
      (fun (experiment_id, start, duration, description, max, min, overbook) ->
        let open CCOption in
        let open Pool_common.Utils in
        let session =
          Session.
            { start = Start.create start |> get_or_failwith
            ; duration = Duration.create duration |> get_or_failwith
            ; description =
                (description
                >>= fun d -> d |> Description.create |> CCResult.to_opt)
            ; max_participants = ParticipantAmount.create max |> get_or_failwith
            ; min_participants = ParticipantAmount.create min |> get_or_failwith
            ; overbook = ParticipantAmount.create overbook |> get_or_failwith
            }
        in
        Session.Created
          (session, Pool_common.Id.of_string experiment_id, location))
      data
  in
  let%lwt () = Lwt_list.iter_s (Session.handle_event pool) events in
  Lwt.return_unit
;;
