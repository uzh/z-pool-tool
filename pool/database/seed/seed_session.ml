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
      , 4
      , None
      , Some 3600
      , None )
    ; ( ex.Experiment.id |> Pool_common.Id.value
      , Ptime.add_span (Ptime_clock.now ()) hour
        |> CCOption.get_exn_or "Invalid time"
      , halfhour
      , Some "No metal allowed!"
      , 28
      , 20
      , 0
      , None
      , None
      , None )
    ; ( ex.Experiment.id |> Pool_common.Id.value
      , Ptime.add_span (Ptime_clock.now ()) hour
        |> CCOption.get_exn_or "Invalid time"
      , halfhour
      , Some "No metal allowed!"
      , 30
      , 2
      , 5
      , None
      , Some 7200
      , None )
    ]
  in
  let events =
    CCList.map
      (fun ( experiment_id
           , start
           , duration
           , description
           , max
           , min
           , overbook
           , reminder_text
           , reminder_lead_time
           , reminder_language ) ->
        let open CCOption in
        let open Pool_common.Utils in
        let session =
          Session.
            { start = Start.create start
            ; duration = Duration.create duration |> get_or_failwith
            ; description =
                (description
                >>= fun d -> d |> Description.create |> CCResult.to_opt)
            ; max_participants = ParticipantAmount.create max |> get_or_failwith
            ; min_participants = ParticipantAmount.create min |> get_or_failwith
            ; overbook = ParticipantAmount.create overbook |> get_or_failwith
            ; reminder_text =
                reminder_text
                |> CCOption.map (fun t ->
                       t |> Pool_common.Reminder.Text.create |> get_or_failwith)
            ; reminder_lead_time =
                reminder_lead_time
                |> CCOption.map (fun lead_time ->
                       lead_time
                       |> Ptime.Span.of_int_s
                       |> Pool_common.Reminder.LeadTime.create
                       |> get_or_failwith)
            ; reminder_language
            }
        in
        Session.Created
          (session, Pool_common.Id.of_string experiment_id, location))
      data
  in
  let%lwt () = Lwt_list.iter_s (Session.handle_event pool) events in
  Lwt.return_unit
;;
