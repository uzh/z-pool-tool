let send_mail =
  let help =
    {|<sender> <recipient>

Provide all fields to send a test email:
        <sender>              : string
        <recipient>           : string

Example: test.mail admin@mail.com contact@mail.com
    |}
  in
  Sihl.Command.make
    ~name:"test.mail"
    ~description:"Dispatch pre-defined test email to the provided recipient"
    ~help
    (function
    | [ sender; recipient ] ->
      let%lwt (_ : Pool_database.Label.t list) =
        Command_utils.setup_databases ()
      in
      let message = "Hi! \n\n This is a test message." in
      let subject = "Test subject" in
      let email = Sihl_email.create ~sender ~recipient ~subject message in
      let%lwt () = Email.Service.dispatch Pool_database.root (email, None) in
      Lwt.return_some ()
    | _ -> Command_utils.failwith_missmatch help)
;;
