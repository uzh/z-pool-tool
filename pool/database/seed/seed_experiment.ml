let get_or_failwith = Pool_common.Utils.get_or_failwith

let experiments pool =
  let data =
    [ ( "The Twenty pound auction"
      , "the_twenty_pound_auction"
      , "It was great fun." )
    ; ( "The Wallet Game"
      , "the_wallet_game"
      , "Students bid for an object in a first-price auction. Each receives an \
         independently drawn signal of the value of the object. The actual \
         value is the sum of the signal." )
    ; ( "The Ultimatum and the Dictator Bargaining Games"
      , "the_ultimatum_and_the_dictator_bargaining_games"
      , "The experiment illustrates the problem of public good provision as \
         discussed in most microeconomics lectures or lectures on public \
         economics." )
    ]
  in
  let events =
    CCList.map
      (fun (title, public_title, description) ->
        let experiment =
          let title = Experiment.Title.create title |> get_or_failwith in
          let public_title =
            Experiment.PublicTitle.create public_title |> get_or_failwith
          in
          let description =
            Experiment.Description.create description |> get_or_failwith
          in
          let direct_registration_disabled =
            Experiment.DirectRegistrationDisabled.create false
          in
          let registration_disabled =
            Experiment.RegistrationDisabled.create false
          in
          Experiment.
            { title
            ; public_title
            ; description
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
