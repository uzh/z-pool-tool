let send_mail =
  let help =
    {|<sender> <recipient> [<count>]

Provide all fields to send a test email:
        <sender>              : string
        <recipient>           : string
        <count>               : int, number of emails (default 1)

Example: test.mail admin@mail.com contact@mail.com
    |}
  in
  Sihl.Command.make
    ~name:"test.mail"
    ~description:"Dispatch pre-defined test email to the provided recipient"
    ~help
    (fun args ->
       let send sender recipient count =
         let%lwt () = Database.Pool.initialize () in
         let message = "Hi! \n\n This is a test message." in
         let subject = "Test subject" in
         let email = Sihl_email.create ~sender ~recipient ~subject message in
         let%lwt () =
           CCList.range' 0 count
           |> Lwt_list.iter_s (fun _ ->
             Email.Service.Job.create email
             |> Email.Service.dispatch Database.Pool.Root.label)
         in
         Lwt.return_some ()
       in
       match args with
       | [ sender; recipient ] -> send sender recipient 1
       | [ sender; recipient; count ] ->
         (match int_of_string_opt count with
          | Some count when count > 0 -> send sender recipient count
          | Some _ | None -> Command_utils.failwith_missmatch help)
       | _ -> Command_utils.failwith_missmatch help)
;;
