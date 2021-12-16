let sign_up =
  Sihl.Command.make
    ~name:"participant.signup"
    ~description:"New participant signup"
    ~help:
      "<Pool_database> <email> <password> <firstname> <lastname> \
       <recruitment_channel> <terms_excepted>"
    (fun args ->
      let return = Lwt.return_some () in
      let help_text =
        {|Provide all fields to sign up a new participant:
    <Pool_database>       : string
    <email>               : string
    <password>            : string
    <firstname>           : string
    <lastname>            : string
    <recruitment_channel> : string of 'friend', 'online', 'lecture', 'mailing'
    <terms_accepted>      : string 'accept' everything else is treated as declined

Example: sihl participant.signup econ-uzh example@mail.com securePassword Max Muster online

Note: Make sure 'accept' is added as final argument, otherwise signup fails.
          |}
      in
      match args with
      | [ db_pool
        ; email
        ; password
        ; firstname
        ; lastname
        ; recruitment_channel
        ; terms_accepted
        ]
        when CCString.equal terms_accepted "accept" ->
        let db_pool =
          Pool_database.Label.create db_pool
          |> CCResult.map_err Pool_common.(Utils.error_to_string Language.En)
          |> CCResult.get_or_failwith
        in
        Database.Root.setup ();
        let%lwt available_pools = Database.Tenant.setup () in
        if CCList.mem ~eq:Pool_database.Label.equal db_pool available_pools
        then (
          let events =
            let open CCResult.Infix in
            Cqrs_command.Participant_command.SignUp.decode
              [ "email", [ email ]
              ; "password", [ password ]
              ; "firstname", [ firstname ]
              ; "lastname", [ lastname ]
              ; "recruitment_channel", [ recruitment_channel ]
              ]
            >>= Cqrs_command.Participant_command.SignUp.handle
            |> CCResult.map_err Pool_common.(Utils.error_to_string Language.En)
            |> CCResult.get_or_failwith
          in
          let%lwt handle_event =
            Lwt_list.iter_s (Pool_event.handle_event db_pool) events
          in
          Lwt.return_some handle_event)
        else (
          print_endline "The specified database pool is not available.";
          return)
      | _ ->
        print_endline help_text;
        return)
;;
