module SmtpAuth = Entity.SmtpAuth
module AccountMap = CCMap.Make (Pool_database.Label)
module Queue = Sihl_queue.MariaDb

module Email = struct
  let dev_inbox : Sihl.Contract.Email.t list ref = ref []
  let accounts : SmtpAuth.Write.t AccountMap.t ref = ref AccountMap.empty

  let remove_from_cache label =
    accounts := AccountMap.remove label !accounts;
    ()
  ;;

  module DevInbox = struct
    let inbox () = !dev_inbox
    let add_to_inbox email = dev_inbox := List.cons email !dev_inbox
    let clear_inbox () = dev_inbox := []
  end

  let print ?(log_level = Logs.Debug) email =
    let open Sihl.Contract.Email in
    Logs.msg log_level (fun m ->
      m
        {|
-----------------------
Email sent by: %s
Recipient: %s
Subject: %s
-----------------------
Text:

%s
-----------------------
Html:

%s
-----------------------
          |}
        email.sender
        email.recipient
        email.subject
        email.text
        (CCOption.value ~default:"<None>" email.html))
  ;;

  let bypass () =
    CCOption.get_or
      ~default:false
      (Sihl.Configuration.read_bool "EMAIL_BYPASS_INTERCEPT")
  ;;

  let intercept_email_address () = Sihl.Configuration.read_string "TEST_EMAIL"

  let console () =
    CCOption.get_or
      ~default:(Sihl.Configuration.is_development ())
      (Sihl.Configuration.read_bool "EMAIL_CONSOLE")
  ;;

  let redirected_email
    new_recipient
    (Sihl_email.{ recipient; subject; cc; bcc; text; _ } as email)
    =
    let subject =
      Format.asprintf
        "[Pool Tool] %s (original to: %s%s)"
        subject
        recipient
        (if cc @ bcc |> CCList.is_empty |> not then ", DEL CC/BCC" else "")
    in
    Sihl_email.
      { email with
        subject
      ; recipient = new_recipient
      ; cc = []
      ; bcc = []
      ; text =
          Format.asprintf
            "DELETED CC: %s\nDELETED BCC: %s\n\n%s"
            ([%show: string list] cc)
            ([%show: string list] bcc)
            text
      }
  ;;

  let intercept_prepare email =
    let () = if console () then print ~log_level:Logs.Info email else () in
    match Sihl.Configuration.is_production (), intercept_email_address () with
    | true, _ -> Ok email
    | false, Some new_recipient ->
      Logs.info (fun m ->
        m
          "Sending email intercepted. Sending email to new recipient ('%s')"
          new_recipient);
      email |> redirected_email new_recipient |> CCResult.pure
    | false, None ->
      Error
        "Sending email intercepted! As no redirect email is specified it/they \
         wont be sent. Please define environment variable 'TEST_EMAIL'."
  ;;

  let intercept_send sender email =
    let () = if console () then print email else () in
    match Sihl.Configuration.is_production (), bypass () with
    | true, _ | _, true -> sender email
    | false, false ->
      Logs.info (fun m ->
        m "Sending email intercepted (non production environment -> DevInbox).");
      email |> DevInbox.add_to_inbox |> Lwt.return
  ;;

  module Smtp = struct
    include DevInbox

    type prepared =
      { sender : string
      ; recipients : Letters.recipient list
      ; subject : string
      ; body : Letters.body
      ; config : Letters.Config.t
      }

    let prepare
      database_label
      { Sihl.Contract.Email.sender; recipient; subject; text; html; cc; bcc }
      =
      let open CCFun in
      let open Utils.Lwt_result.Infix in
      let open SmtpAuth in
      let recipients =
        CCList.concat
          [ [ Letters.To recipient ]
          ; CCList.map (fun address -> Letters.Cc address) cc
          ; CCList.map (fun address -> Letters.Bcc address) bcc
          ]
      in
      let body =
        match html with
        | Some html -> Letters.Html html
        | None -> Letters.Plain text
      in
      let%lwt config =
        let open Pool_common.Utils in
        try !accounts |> AccountMap.find database_label |> Lwt.return with
        | _ ->
          let%lwt from_repo =
            Repo.Smtp.find_full_by_label database_label
            >|- with_log_error
            ||> get_or_failwith
          in
          accounts := AccountMap.add database_label from_repo !accounts;
          Lwt.return from_repo
      in
      let username = CCOption.(config.Write.username >|= Username.value) in
      let password = CCOption.(config.Write.password >|= Password.value) in
      let hostname = Server.value config.Write.server in
      let port = Some (Port.value config.Write.port) in
      let mechanism =
        if Mechanism.(equal LOGIN) config.Write.mechanism
           && (CCOption.is_none username || CCOption.is_none password)
        then
          raise
            (Sihl.Contract.Email.Exception
               "SMTP auth mechanism cannot be set to LOGIN when no username or \
                password is set.")
        else Mechanism.to_sendmail config.Write.mechanism
      in
      let with_starttls = Protocol.(equal STARTTLS) config.Write.protocol in
      let config =
        Letters.Config.create
          ~mechanism
          ~username:(CCOption.get_or ~default:"" username)
          ~password:(CCOption.get_or ~default:"" password)
          ~hostname
          ~with_starttls
          ()
        |> Letters.Config.set_port port
      in
      let%lwt sender =
        let valid_email =
          let open Re in
          (* Checks for more than 1 character before and more than 2 characters
             after the @ sign *)
          seq [ repn any 1 None; char '@'; repn any 2 None ]
          |> whole_string
          |> compile
          |> Re.execp
        in
        let%lwt sender_of_pool =
          if Pool_database.is_root database_label
          then Lwt.return (Sihl.Configuration.read_string "SMTP_SENDER")
          else
            Settings.find_contact_email database_label
            ||> Settings.ContactEmail.value
            ||> CCOption.pure
        in
        [ username; sender_of_pool ]
        |> CCList.filter (CCOption.map_or ~default:false valid_email)
        |> CCOption.choice
        |> CCOption.get_or ~default:sender
        |> Lwt.return
      in
      Lwt.return { sender; recipients; subject; body; config }
    ;;

    let send database_label email =
      let%lwt { sender; recipients; subject; body; config } =
        prepare database_label email
      in
      Letters.build_email ~from:sender ~recipients ~subject ~body
      |> function
      | Ok message ->
        Logs.info (fun m ->
          m "Send email as %s to %s" sender email.Sihl_email.recipient);
        Letters.send ~config ~sender ~recipients ~message
      | Error msg -> raise (Sihl.Contract.Email.Exception msg)
    ;;
  end

  let send database_label = intercept_send (Smtp.send database_label)
  let start () = Lwt.return_unit
  let stop () = Lwt.return_unit

  let lifecycle =
    Sihl.Container.create_lifecycle
      Sihl.Contract.Email.name
      ~dependencies:(fun () -> [ Sihl.Database.lifecycle ])
      ~start
      ~stop
  ;;

  let register () =
    let configuration = Sihl.Configuration.make () in
    Sihl.Container.Service.create ~configuration lifecycle
  ;;

  module Job = struct
    let send =
      let open CCFun in
      let open Utils.Lwt_result.Infix in
      let handle ?ctx email =
        let database_label =
          let open CCOption in
          ctx
          >>= CCList.assoc_opt ~eq:( = ) "pool"
          |> function
          | Some context ->
            Pool_database.Label.create context
            |> Pool_common.Utils.get_or_failwith
          | None -> raise (Failure "Invalid context passed!")
        in
        Lwt.catch
          (fun () -> send database_label email ||> CCResult.pure)
          (Printexc.to_string %> Lwt.return_error)
      in
      let encode = Sihl.Contract.Email.to_yojson %> Yojson.Safe.to_string in
      let decode email =
        let open CCResult.Infix in
        (try Ok (Yojson.Safe.from_string email) with
         | _ ->
           Logs.err (fun m ->
             m
               "Serialized email string was NULL, can not deserialize email. \
                Please fix the string manually and reset the job instance.");
           Error "Invalid serialized email string received")
        >>= Sihl.Contract.Email.of_yojson
            %> CCOption.to_result "Failed to deserialize email"
      in
      Sihl.Contract.Queue.create_job
        handle
        ~max_tries:10
        ~retry_delay:(Sihl.Time.Span.hours 1)
        encode
        decode
        "send_email"
    ;;
  end

  let dispatch database_label email =
    Logs.info (fun m -> m "Dispatch email to %s" email.Sihl_email.recipient);
    print email;
    Queue.dispatch
      ~ctx:(Entity.to_ctx database_label)
      (email |> intercept_prepare |> CCResult.get_or_failwith)
      Job.send
  ;;

  let dispatch_all database_label emails =
    let recipients = CCList.map (fun m -> m.Sihl_email.recipient) emails in
    Logs.info (fun m ->
      m "Dispatch email to %s" ([%show: string list] recipients));
    Queue.dispatch_all ~ctx:(Entity.to_ctx database_label) emails Job.send
  ;;
end
