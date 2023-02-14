module Reminder = Pool_common.Reminder

let get_or_failwith = Pool_common.Utils.get_or_failwith

let experiments pool =
  let open CCFun in
  let open CCOption.Infix in
  let data =
    [ ( "The Twenty pound auction"
      , "Trading experiment"
      , "It was great fun."
      , false
      , Some (60 * 60) )
    ; ( "The Wallet Game"
      , "Finance experiment"
      , "Students bid for an object in a first-price auction. Each receives an \
         independently drawn signal of the value of the object. The actual \
         value is the sum of the signal."
      , false
      , None )
    ; ( "The Ultimatum and the Dictator Bargaining Games"
      , "Bidding experiment"
      , "The experiment illustrates the problem of public good provision as \
         discussed in most microeconomics lectures or lectures on public \
         economics."
      , true
      , Some (60 * 60) )
    ; ( "4"
      , "Finance experiment"
      , "Students bid for an object in a first-price auction. Each receives an \
         independently drawn signal of the value of the object. The actual \
         value is the sum of the signal."
      , false
      , None )
    ; ( "5"
      , "Finance experiment"
      , "Students bid for an object in a first-price auction. Each receives an \
         independently drawn signal of the value of the object. The actual \
         value is the sum of the signal."
      , false
      , None )
    ; ( "6"
      , "Finance experiment"
      , "Students bid for an object in a first-price auction. Each receives an \
         independently drawn signal of the value of the object. The actual \
         value is the sum of the signal."
      , false
      , None )
    ; ( "7"
      , "Finance experiment"
      , "Students bid for an object in a first-price auction. Each receives an \
         independently drawn signal of the value of the object. The actual \
         value is the sum of the signal."
      , false
      , None )
    ; ( "8"
      , "Finance experiment"
      , "Students bid for an object in a first-price auction. Each receives an \
         independently drawn signal of the value of the object. The actual \
         value is the sum of the signal."
      , false
      , None )
    ; ( "9"
      , "Finance experiment"
      , "Students bid for an object in a first-price auction. Each receives an \
         independently drawn signal of the value of the object. The actual \
         value is the sum of the signal."
      , false
      , None )
    ; ( "9"
      , "Finance experiment"
      , "Students bid for an object in a first-price auction. Each receives an \
         independently drawn signal of the value of the object. The actual \
         value is the sum of the signal."
      , false
      , None )
    ; ( "10"
      , "Finance experiment"
      , "Students bid for an object in a first-price auction. Each receives an \
         independently drawn signal of the value of the object. The actual \
         value is the sum of the signal."
      , false
      , None )
    ; ( "11"
      , "Finance experiment"
      , "Students bid for an object in a first-price auction. Each receives an \
         independently drawn signal of the value of the object. The actual \
         value is the sum of the signal."
      , false
      , None )
    ; ( "12"
      , "Finance experiment"
      , "Students bid for an object in a first-price auction. Each receives an \
         independently drawn signal of the value of the object. The actual \
         value is the sum of the signal."
      , false
      , None )
    ; ( "13"
      , "Finance experiment"
      , "Students bid for an object in a first-price auction. Each receives an \
         independently drawn signal of the value of the object. The actual \
         value is the sum of the signal."
      , false
      , None )
    ; ( "14"
      , "Finance experiment"
      , "Students bid for an object in a first-price auction. Each receives an \
         independently drawn signal of the value of the object. The actual \
         value is the sum of the signal."
      , false
      , None )
    ; ( "15"
      , "Finance experiment"
      , "Students bid for an object in a first-price auction. Each receives an \
         independently drawn signal of the value of the object. The actual \
         value is the sum of the signal."
      , false
      , None )
    ; ( "16"
      , "Finance experiment"
      , "Students bid for an object in a first-price auction. Each receives an \
         independently drawn signal of the value of the object. The actual \
         value is the sum of the signal."
      , false
      , None )
    ; ( "17"
      , "Finance experiment"
      , "Students bid for an object in a first-price auction. Each receives an \
         independently drawn signal of the value of the object. The actual \
         value is the sum of the signal."
      , false
      , None )
    ; ( "18"
      , "Finance experiment"
      , "Students bid for an object in a first-price auction. Each receives an \
         independently drawn signal of the value of the object. The actual \
         value is the sum of the signal."
      , false
      , None )
    ; ( "19"
      , "Finance experiment"
      , "Students bid for an object in a first-price auction. Each receives an \
         independently drawn signal of the value of the object. The actual \
         value is the sum of the signal."
      , false
      , None )
    ; ( "20"
      , "Finance experiment"
      , "Students bid for an object in a first-price auction. Each receives an \
         independently drawn signal of the value of the object. The actual \
         value is the sum of the signal."
      , false
      , None )
    ; ( "21"
      , "Finance experiment"
      , "Students bid for an object in a first-price auction. Each receives an \
         independently drawn signal of the value of the object. The actual \
         value is the sum of the signal."
      , false
      , None )
    ; ( "22"
      , "Finance experiment"
      , "Students bid for an object in a first-price auction. Each receives an \
         independently drawn signal of the value of the object. The actual \
         value is the sum of the signal."
      , false
      , None )
    ; ( "23"
      , "Finance experiment"
      , "Students bid for an object in a first-price auction. Each receives an \
         independently drawn signal of the value of the object. The actual \
         value is the sum of the signal."
      , false
      , None )
    ; ( "24"
      , "Finance experiment"
      , "Students bid for an object in a first-price auction. Each receives an \
         independently drawn signal of the value of the object. The actual \
         value is the sum of the signal."
      , false
      , None )
    ; ( "25"
      , "Finance experiment"
      , "Students bid for an object in a first-price auction. Each receives an \
         independently drawn signal of the value of the object. The actual \
         value is the sum of the signal."
      , false
      , None )
    ; ( "26"
      , "Finance experiment"
      , "Students bid for an object in a first-price auction. Each receives an \
         independently drawn signal of the value of the object. The actual \
         value is the sum of the signal."
      , false
      , None )
    ; ( "27"
      , "Finance experiment"
      , "Students bid for an object in a first-price auction. Each receives an \
         independently drawn signal of the value of the object. The actual \
         value is the sum of the signal."
      , false
      , None )
    ; ( "28"
      , "Finance experiment"
      , "Students bid for an object in a first-price auction. Each receives an \
         independently drawn signal of the value of the object. The actual \
         value is the sum of the signal."
      , false
      , None )
    ; ( "29"
      , "Finance experiment"
      , "Students bid for an object in a first-price auction. Each receives an \
         independently drawn signal of the value of the object. The actual \
         value is the sum of the signal."
      , false
      , None )
    ; ( "30"
      , "Finance experiment"
      , "Students bid for an object in a first-price auction. Each receives an \
         independently drawn signal of the value of the object. The actual \
         value is the sum of the signal."
      , false
      , None )
    ; ( "29"
      , "Finance experiment"
      , "Students bid for an object in a first-price auction. Each receives an \
         independently drawn signal of the value of the object. The actual \
         value is the sum of the signal."
      , false
      , None )
    ; ( "29"
      , "Finance experiment"
      , "Students bid for an object in a first-price auction. Each receives an \
         independently drawn signal of the value of the object. The actual \
         value is the sum of the signal."
      , false
      , None )
    ; ( "29"
      , "Finance experiment"
      , "Students bid for an object in a first-price auction. Each receives an \
         independently drawn signal of the value of the object. The actual \
         value is the sum of the signal."
      , false
      , None )
    ; ( "29"
      , "Finance experiment"
      , "Students bid for an object in a first-price auction. Each receives an \
         independently drawn signal of the value of the object. The actual \
         value is the sum of the signal."
      , false
      , None )
    ; ( "29"
      , "Finance experiment"
      , "Students bid for an object in a first-price auction. Each receives an \
         independently drawn signal of the value of the object. The actual \
         value is the sum of the signal."
      , false
      , None )
    ; ( "29"
      , "Finance experiment"
      , "Students bid for an object in a first-price auction. Each receives an \
         independently drawn signal of the value of the object. The actual \
         value is the sum of the signal."
      , false
      , None )
    ; ( "29"
      , "Finance experiment"
      , "Students bid for an object in a first-price auction. Each receives an \
         independently drawn signal of the value of the object. The actual \
         value is the sum of the signal."
      , false
      , None )
    ; ( "29"
      , "Finance experiment"
      , "Students bid for an object in a first-price auction. Each receives an \
         independently drawn signal of the value of the object. The actual \
         value is the sum of the signal."
      , false
      , None )
    ; ( "29"
      , "Finance experiment"
      , "Students bid for an object in a first-price auction. Each receives an \
         independently drawn signal of the value of the object. The actual \
         value is the sum of the signal."
      , false
      , None )
    ; ( "29"
      , "Finance experiment"
      , "Students bid for an object in a first-price auction. Each receives an \
         independently drawn signal of the value of the object. The actual \
         value is the sum of the signal."
      , false
      , None )
    ; ( "29"
      , "Finance experiment"
      , "Students bid for an object in a first-price auction. Each receives an \
         independently drawn signal of the value of the object. The actual \
         value is the sum of the signal."
      , false
      , None )
    ; ( "29"
      , "Finance experiment"
      , "Students bid for an object in a first-price auction. Each receives an \
         independently drawn signal of the value of the object. The actual \
         value is the sum of the signal."
      , false
      , None )
    ; ( "29"
      , "Finance experiment"
      , "Students bid for an object in a first-price auction. Each receives an \
         independently drawn signal of the value of the object. The actual \
         value is the sum of the signal."
      , false
      , None )
    ; ( "29"
      , "Finance experiment"
      , "Students bid for an object in a first-price auction. Each receives an \
         independently drawn signal of the value of the object. The actual \
         value is the sum of the signal."
      , false
      , None )
    ; ( "29"
      , "Finance experiment"
      , "Students bid for an object in a first-price auction. Each receives an \
         independently drawn signal of the value of the object. The actual \
         value is the sum of the signal."
      , false
      , None )
    ; ( "29"
      , "Finance experiment"
      , "Students bid for an object in a first-price auction. Each receives an \
         independently drawn signal of the value of the object. The actual \
         value is the sum of the signal."
      , false
      , None )
    ; ( "29"
      , "Finance experiment"
      , "Students bid for an object in a first-price auction. Each receives an \
         independently drawn signal of the value of the object. The actual \
         value is the sum of the signal."
      , false
      , None )
    ; ( "29"
      , "Finance experiment"
      , "Students bid for an object in a first-price auction. Each receives an \
         independently drawn signal of the value of the object. The actual \
         value is the sum of the signal."
      , false
      , None )
    ; ( "29"
      , "Finance experiment"
      , "Students bid for an object in a first-price auction. Each receives an \
         independently drawn signal of the value of the object. The actual \
         value is the sum of the signal."
      , false
      , None )
    ; ( "29"
      , "Finance experiment"
      , "Students bid for an object in a first-price auction. Each receives an \
         independently drawn signal of the value of the object. The actual \
         value is the sum of the signal."
      , false
      , None )
    ; ( "29"
      , "Finance experiment"
      , "Students bid for an object in a first-price auction. Each receives an \
         independently drawn signal of the value of the object. The actual \
         value is the sum of the signal."
      , false
      , None )
    ]
  in
  let events =
    CCList.map
      (fun ( title
           , public_title
           , description
           , direct_registration_disabled
           , session_reminder_lead_time ) ->
        let experiment =
          let open Experiment in
          let title = Title.create title |> get_or_failwith in
          let public_title =
            PublicTitle.create public_title |> get_or_failwith
          in
          let description = Description.create description |> get_or_failwith in
          let session_reminder_lead_time =
            session_reminder_lead_time
            >|= Ptime.Span.of_int_s
                %> Reminder.LeadTime.create
                %> get_or_failwith
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
            session_reminder_lead_time
          |> get_or_failwith
        in
        Experiment.Created experiment)
      data
  in
  let%lwt () = Lwt_list.iter_s (Experiment.handle_event pool) events in
  Lwt.return_unit
;;
