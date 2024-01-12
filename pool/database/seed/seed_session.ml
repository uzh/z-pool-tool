module Reminder = Pool_common.Reminder

let halfhour, hour = CCPair.map_same Ptime.Span.of_int_s (30 * 60, 60 * 60)

let session_data =
  [ ( Ptime.add_span (Ptime_clock.now ()) hour
      |> CCOption.get_exn_or "Invalid time"
    , hour
    , None
    , Some "Must be healthy"
    , 30
    , 4
    , 4
    , Some 3600
    , Some 1800 )
  ; ( Ptime.add_span (Ptime_clock.now ()) halfhour
      |> CCOption.get_exn_or "Invalid time"
    , halfhour
    , Some "No metal allowed!"
    , None
    , 28
    , 20
    , 0
    , None
    , None )
  ; ( Ptime.add_span (Ptime_clock.now ()) hour
      |> CCOption.get_exn_or "Invalid time"
    , halfhour
    , Some "No metal allowed!"
    , None
    , 30
    , 2
    , 5
    , Some 7200
    , None )
  ]
;;

let create pool =
  let open CCFun in
  let open Pool_common.Utils in
  let%lwt experiments, (_ : Query.t) = Experiment.find_all pool in
  let%lwt locations = Pool_location.find_all pool in
  let%lwt () =
    Lwt_list.iter_s
      (fun experiment ->
        let main_session_events =
          CCList.map
            (fun ( start
                 , duration
                 , internal_description
                 , public_description
                 , max
                 , min
                 , overbook
                 , email_reminder_lead_time
                 , text_message_reminder_lead_time ) ->
              let open CCOption in
              let (session : Session.t) =
                let open Session in
                let start = Start.create start in
                let duration = Duration.create duration |> get_or_failwith in
                let internal_description =
                  internal_description
                  >>= InternalDescription.create %> of_result
                in
                let public_description =
                  public_description >>= PublicDescription.create %> of_result
                in
                let max_participants =
                  ParticipantAmount.create max |> get_or_failwith
                in
                let min_participants =
                  ParticipantAmount.create min |> get_or_failwith
                in
                let overbook =
                  ParticipantAmount.create overbook |> get_or_failwith
                in
                let create_lead_time seconds decoder =
                  seconds
                  >|= Ptime.Span.of_int_s
                  >|= decoder
                  >|= get_or_failwith
                in
                let email_reminder_lead_time =
                  create_lead_time
                    email_reminder_lead_time
                    Reminder.EmailLeadTime.create
                in
                let text_message_reminder_lead_time =
                  create_lead_time
                    text_message_reminder_lead_time
                    Reminder.TextMessageLeadTime.create
                in
                let location = CCList.hd locations in
                create
                  ?internal_description
                  ?public_description
                  ?email_reminder_lead_time
                  ?text_message_reminder_lead_time
                  start
                  duration
                  location
                  max_participants
                  min_participants
                  overbook
              in
              Session.Created (session, experiment.Experiment.id))
            session_data
        in
        Lwt_list.iter_s (Session.handle_event pool) main_session_events)
      experiments
  in
  Lwt_list.iter_s
    (fun experiment ->
      let%lwt sessions =
        Session.find_all_for_experiment pool experiment.Experiment.id
      in
      let parent = CCList.hd sessions in
      let (follow_up : Session.t) =
        let open CCOption in
        let open Session in
        let start =
          Ptime.add_span (parent.start |> Session.Start.value) hour
          |> CCOption.get_exn_or "Invalid time"
          |> Start.create
        in
        let duration = Duration.create halfhour |> get_or_failwith in
        let internal_description =
          Some "MRI Study" >>= InternalDescription.create %> of_result
        in
        Session.create
          ?internal_description
          ~follow_up_to:parent.Session.id
          start
          duration
          parent.location
          parent.max_participants
          parent.min_participants
          parent.overbook
      in
      Session.handle_event pool
      @@ Session.Created (follow_up, experiment.Experiment.id))
    experiments
;;
