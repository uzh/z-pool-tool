let halfhour, hour = CCPair.map_same Ptime.Span.of_int_s (30 * 60, 60 * 60)

let session_data =
  [ ( Ptime.add_span (Ptime_clock.now ()) hour
      |> CCOption.get_exn_or "Invalid time"
    , hour
    , None
    , 30
    , 4
    , 4
    , Some 3600 )
  ; ( Ptime.add_span (Ptime_clock.now ()) halfhour
      |> CCOption.get_exn_or "Invalid time"
    , halfhour
    , Some "No metal allowed!"
    , 28
    , 20
    , 0
    , None )
  ; ( Ptime.add_span (Ptime_clock.now ()) hour
      |> CCOption.get_exn_or "Invalid time"
    , halfhour
    , Some "No metal allowed!"
    , 30
    , 2
    , 5
    , Some 7200 )
  ]
;;

let create pool =
  let open CCFun in
  let open Pool_common.Utils in
  let%lwt experiments = Experiment.find_all pool () in
  let%lwt locations = Pool_location.find_all pool in
  let%lwt () =
    Lwt_list.iter_s
      (fun experiment ->
        let main_session_events =
          CCList.map
            (fun ( start
                 , duration
                 , description
                 , max
                 , min
                 , overbook
                 , reminder_lead_time ) ->
              let open CCOption in
              let (session : Session.base) =
                Session.
                  { start = Start.create start
                  ; duration = Duration.create duration |> get_or_failwith
                  ; description =
                      description >>= Description.create %> of_result
                  ; max_participants =
                      ParticipantAmount.create max |> get_or_failwith
                  ; min_participants =
                      ParticipantAmount.create min |> get_or_failwith
                  ; overbook =
                      ParticipantAmount.create overbook |> get_or_failwith
                  ; reminder_lead_time =
                      reminder_lead_time
                      >|= Ptime.Span.of_int_s
                          %> Pool_common.Reminder.LeadTime.create
                          %> get_or_failwith
                  }
              in
              let location = CCList.hd locations in
              Session.Created (session, None, experiment.Experiment.id, location))
            session_data
        in
        Lwt_list.iter_s (Session.handle_event pool) main_session_events)
      experiments
  in
  Lwt_list.iter_s
    (fun experiment ->
      let%lwt sessions =
        let open Utils.Lwt_result.Infix in
        Session.find_all_for_experiment pool experiment.Experiment.id
        ||> CCResult.get_exn
      in
      let parent = CCList.hd sessions in
      let (follow_up : Session.base) =
        let open CCOption in
        Session.
          { start =
              Start.create
                (Ptime.add_span (parent.start |> Session.Start.value) hour
                |> CCOption.get_exn_or "Invalid time")
          ; duration = Duration.create halfhour |> get_or_failwith
          ; description = Some "MRI Study" >>= Description.create %> of_result
          ; max_participants = parent.max_participants
          ; min_participants = parent.min_participants
          ; overbook = parent.overbook
          ; reminder_lead_time = None
          }
      in
      let location = CCList.hd locations in
      Session.handle_event pool
      @@ Session.Created
           ( follow_up
           , Some parent.Session.id
           , experiment.Experiment.id
           , location ))
    experiments
;;
