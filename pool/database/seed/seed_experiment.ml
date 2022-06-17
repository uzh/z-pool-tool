let get_or_failwith = Pool_common.Utils.get_or_failwith

let experiments pool =
  let data =
    [ "The Twenty pound auction", "It was great fun.", Some (60 * 60), None
    ; ( "The Wallet Game"
      , "Students bid for an object in a first-price auction. Each receives an \
         independently drawn signal of the value of the object. The actual \
         value is the sum of the signal."
      , None
      , None )
    ; ( "The Ultimatum and the Dictator Bargaining Games"
      , "The experiment illustrates the problem of public good provision as \
         discussed in most microeconomics lectures or lectures on public \
         economics."
      , Some (60 * 60)
      , Some "Don't forget your session." )
    ]
  in
  let events =
    CCList.map
      (fun ( title
           , description
           , session_reminder_lead_time
           , session_reminder_text ) ->
        let experiment =
          let title = Experiment.Title.create title |> get_or_failwith in
          let description =
            Experiment.Description.create description |> get_or_failwith
          in
          let session_reminder_lead_time =
            session_reminder_lead_time
            |> CCOption.map (fun t ->
                   Ptime.Span.of_int_s @@ t
                   |> Pool_common.Reminder.LeadTime.create
                   |> get_or_failwith)
          in
          let session_reminder_text =
            session_reminder_text
            |> CCOption.map (fun t ->
                   t |> Pool_common.Reminder.Text.create |> get_or_failwith)
          in
          let waiting_list_disabled =
            Experiment.WaitingListDisabled.create true
          in
          let direct_registration_disabled =
            Experiment.DirectRegistrationDisabled.create false
          in
          let registration_disabled =
            Experiment.RegistrationDisabled.create false
          in
          Experiment.
            { title
            ; description
            ; session_reminder_lead_time
            ; session_reminder_text
            ; waiting_list_disabled
            ; direct_registration_disabled
            ; registration_disabled
            }
        in
        Experiment.Created experiment)
      data
  in
  let%lwt () = Lwt_list.iter_s (Experiment.handle_event pool) events in
  Lwt.return_unit
;;
