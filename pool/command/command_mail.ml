let send_mail =
  Sihl.Command.make
    ~name:"mail"
    ~description:"Send pre-defined test email to the provided recipient"
    (fun args ->
    match args with
    | [ sender; recipient ] ->
      let message = "Hi! \n\n This is a test message." in
      let subject = "Test subject" in
      let email = Sihl_email.create ~sender ~recipient ~subject message in
      let%lwt () = Service.Email.send email in
      Lwt.return_some ()
    | _ ->
      print_endline "Provide <sender> and <recipient> email addresses";
      Lwt.return_some ())
;;
