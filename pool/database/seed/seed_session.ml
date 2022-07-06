let create pool =
  let open CCFun in
  let%lwt experiments = Experiment.find_all pool () in
  let%lwt location = Pool_location.find_all pool in
  let location =
    location |> CCList.head_opt |> CCOption.get_exn_or "No locations seeded"
  in
  let experiment_id =
    experiments
    |> CCList.head_opt
    |> CCOption.get_exn_or "No experiments seeded"
    |> fun ex -> ex.Experiment.id
  in
  let halfhour, hour = CCPair.map_same Ptime.Span.of_int_s (30 * 60, 60 * 60) in
  let data =
    [ ( experiment_id
      , Ptime.add_span (Ptime_clock.now ()) hour
        |> CCOption.get_exn_or "Invalid time"
      , hour
      , None
      , 30
      , 4
      , 4
      , None
      , None
      , Some 3600 )
    ; ( experiment_id
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
    ; ( experiment_id
      , Ptime.add_span (Ptime_clock.now ()) hour
        |> CCOption.get_exn_or "Invalid time"
      , halfhour
      , Some "No metal allowed!"
      , 30
      , 2
      , 5
      , None
      , None
      , Some 7200 )
    ]
  in
  let open Pool_common.Utils in
  let main_session_events =
    CCList.map
      (fun ( experiment_id
           , start
           , duration
           , description
           , max
           , min
           , overbook
           , reminder_subject
           , reminder_text
           , reminder_lead_time ) ->
        let open CCOption in
        let session =
          Session.
            { start = Start.create start
            ; duration = Duration.create duration |> get_or_failwith
            ; description = description >>= Description.create %> of_result
            ; max_participants = ParticipantAmount.create max |> get_or_failwith
            ; min_participants = ParticipantAmount.create min |> get_or_failwith
            ; overbook = ParticipantAmount.create overbook |> get_or_failwith
            ; reminder_subject
            ; reminder_text =
                reminder_text
                >|= Pool_common.Reminder.Text.create %> get_or_failwith
            ; reminder_lead_time =
                reminder_lead_time
                >|= Ptime.Span.of_int_s
                    %> Pool_common.Reminder.LeadTime.create
                    %> get_or_failwith
            }
        in
        Session.Created (session, None, experiment_id, location))
      data
  in
  let%lwt () =
    Lwt_list.iter_s (Session.handle_event pool) main_session_events
  in
  let%lwt sessions = Session.find_all_for_experiment pool experiment_id in
  let parent = sessions |> get_or_failwith |> CCList.hd in
  let follow_up =
    let open CCOption in
    Session.
      { start =
          Start.create
            (Ptime.add_span (Ptime_clock.now ()) hour
            |> CCOption.get_exn_or "Invalid time")
      ; duration = Duration.create halfhour |> get_or_failwith
      ; description = Some "MRI Study" >>= Description.create %> of_result
      ; max_participants = ParticipantAmount.create 10 |> get_or_failwith
      ; min_participants = ParticipantAmount.create 2 |> get_or_failwith
      ; overbook = ParticipantAmount.create 3 |> get_or_failwith
      ; reminder_lead_time = None
      ; reminder_subject = None
      ; reminder_text = None
      }
  in
  let%lwt () =
    Session.handle_event pool
    @@ Session.Created
         (follow_up, Some parent.Session.id, experiment_id, location)
  in
  Lwt.return_unit
;;
