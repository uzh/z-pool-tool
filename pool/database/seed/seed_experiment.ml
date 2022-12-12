module Reminder = Pool_common.Reminder

let get_or_failwith = Pool_common.Utils.get_or_failwith

let experiments pool =
  let open CCFun in
  let open CCOption.Infix in
  let data =
    [ ( "The Twenty pound auction"
      , "the_twenty_pound_auction"
      , "It was great fun."
      , None
      , None
      , false
      , Some (60 * 60)
      , None
      , None )
    ; ( "The Wallet Game"
      , "the_wallet_game"
      , "Students bid for an object in a first-price auction. Each receives an \
         independently drawn signal of the value of the object. The actual \
         value is the sum of the signal."
      , None
      , None
      , false
      , None
      , None
      , None )
    ; ( "The Ultimatum and the Dictator Bargaining Games"
      , "the_ultimatum_and_the_dictator_bargaining_games"
      , "The experiment illustrates the problem of public good provision as \
         discussed in most microeconomics lectures or lectures on public \
         economics."
      , None
      , None
      , true
      , Some (60 * 60)
      , Some "Subject"
      , Some "Don't forget your session." )
    ]
  in
  let events =
    CCList.map
      (fun ( title
           , public_title
           , description
           , invitation_subject
           , invitation_text
           , direct_registration_disabled
           , session_reminder_lead_time
           , session_reminder_subject
           , session_reminder_text ) ->
        let experiment =
          let open Experiment in
          let title = Title.create title |> get_or_failwith in
          let public_title =
            PublicTitle.create public_title |> get_or_failwith
          in
          let description = Description.create description |> get_or_failwith in
          let invitation_subject =
            invitation_subject
            >|= InvitationTemplate.Subject.create %> get_or_failwith
          in
          let invitation_text =
            invitation_text
            >|= InvitationTemplate.Text.create %> get_or_failwith
          in
          let session_reminder_lead_time =
            session_reminder_lead_time
            >|= Ptime.Span.of_int_s
                %> Reminder.LeadTime.create
                %> get_or_failwith
          in
          let session_reminder_text =
            session_reminder_text >|= Reminder.Text.create %> get_or_failwith
          in
          let session_reminder_subject =
            session_reminder_subject
            >|= Reminder.Subject.create %> get_or_failwith
          in
          let direct_registration_disabled =
            DirectRegistrationDisabled.create direct_registration_disabled
          in
          let allow_uninvited_signup = AllowUninvitedSignup.create false in
          let registration_disabled = RegistrationDisabled.create false in
          create
            title
            public_title
            description
            direct_registration_disabled
            registration_disabled
            allow_uninvited_signup
            (Some Pool_common.ExperimentType.Lab)
            invitation_subject
            invitation_text
            session_reminder_lead_time
            session_reminder_subject
            session_reminder_text
          |> get_or_failwith
        in
        Experiment.Created experiment)
      data
  in
  let%lwt () = Lwt_list.iter_s (Experiment.handle_event pool) events in
  Lwt.return_unit
;;
