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
      let%lwt () = Database.Pool.initialize () in
      let message = "Hi! \n\n This is a test message." in
      let subject = "Test subject" in
      let email = Sihl_email.create ~sender ~recipient ~subject message in
      let job = Email.Service.Job.create email in
      Database.(transaction_ctx Pool.Root.label) @@ fun db_ctx ->
      let%lwt () = Email.Service.dispatch db_ctx job in
      Lwt.return_some ()
    | _ -> Command_utils.failwith_missmatch help)
;;
