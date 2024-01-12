module Reminder = Pool_common.Reminder

let get_or_failwith = Pool_common.Utils.get_or_failwith

let experiments pool =
  let open CCOption.Infix in
  let data =
    [ ( "The Twenty pound auction"
      , "Trading experiment"
      , "It was great fun."
      , Some "F-00000-11-22"
      , false
      , Some (60 * 60) )
    ; ( "The Wallet Game"
      , "Finance experiment"
      , "Students bid for an object in a first-price auction. Each receives an \
         independently drawn signal of the value of the object. The actual \
         value is the sum of the signal."
      , Some "F-00000-11-22"
      , false
      , None )
    ; ( "The Ultimatum and the Dictator Bargaining Games"
      , "Bidding experiment"
      , "The experiment illustrates the problem of public good provision as \
         discussed in most microeconomics lectures or lectures on public \
         economics."
      , None
      , true
      , Some (60 * 60) )
    ]
  in
  let events =
    CCList.map
      (fun ( title
           , public_title
           , description
           , cost_center
           , direct_registration_disabled
           , email_session_reminder_lead_time ) ->
        let experiment =
          let open Experiment in
          let title = Title.create title |> get_or_failwith in
          let public_title =
            PublicTitle.create public_title |> get_or_failwith
          in
          let internal_description =
            InternalDescription.create description |> get_or_failwith
          in
          let cost_center = cost_center |> CCOption.map CostCenter.of_string in
          let email_session_reminder_lead_time =
            email_session_reminder_lead_time
            >|= Ptime.Span.of_int_s
            >|= Reminder.EmailLeadTime.create
            >|= get_or_failwith
          in
          let direct_registration_disabled =
            DirectRegistrationDisabled.create direct_registration_disabled
          in
          let allow_uninvited_signup = AllowUninvitedSignup.create false in
          let external_data_required = ExternalDataRequired.create false in
          let show_external_data_id_links =
            ShowExternalDataIdLinks.create false
          in
          let registration_disabled = RegistrationDisabled.create false in
          create
            title
            public_title
            ?cost_center
            ~internal_description
            ?email_session_reminder_lead_time
            ~experiment_type:Pool_common.ExperimentType.Lab
            direct_registration_disabled
            registration_disabled
            allow_uninvited_signup
            external_data_required
            show_external_data_id_links
          |> get_or_failwith
        in
        Experiment.Created experiment)
      data
  in
  let%lwt () = Lwt_list.iter_s (Experiment.handle_event pool) events in
  Lwt.return_unit
;;
