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

  let print email =
    let open Sihl.Contract.Email in
    Logs.info (fun m ->
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

  let bypass_email_address () = Sihl.Configuration.read_string "TEST_EMAIL"

  let redirected_email
    new_recipient
    (Sihl_email.{ recipient; subject; _ } as email)
    =
    let subject =
      Format.asprintf "[Pool Tool] %s (original to: %s)" subject recipient
    in
    Sihl_email.{ email with subject; recipient = new_recipient }
  ;;

  let intercept sender email =
    let is_production = Sihl.Configuration.is_production () in
    let bypass = bypass () in
    let bypass_email_address = bypass_email_address () in
    let console =
      CCOption.get_or
        ~default:(Sihl.Configuration.is_development ())
        (Sihl.Configuration.read_bool "EMAIL_CONSOLE")
    in
    let () = if console then print email else () in
    match is_production, bypass_email_address with
    | true, _ -> sender email
    | false, Some new_recipient when bypass ->
      Logs.info (fun m ->
        m
          "Sending email intercepted. Sending email(s) to new recipient ('%s')"
          new_recipient);
      email |> redirected_email new_recipient |> sender
    | false, Some new_recipient ->
      Logs.info (fun m ->
        m
          "Sending email intercepted (non production environment -> DevInbox). \
           Sending email(s) to new recipient ('%s')"
          new_recipient);
      email
      |> redirected_email new_recipient
      |> DevInbox.add_to_inbox
      |> Lwt.return
    | false, None ->
      Logs.err (fun m ->
        m
          "%s"
          "Sending email intercepted! As no redirect email is specified \
           it/they wont be sent. Please define environment variable \
           'TEST_EMAIL'.");
      Lwt.return_unit
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
      Lwt.return { sender; recipients; subject; body; config }
    ;;

    let send' database_label email =
      let%lwt { sender; recipients; subject; body; config } =
        prepare database_label email
      in
      Letters.build_email ~from:sender ~recipients ~subject ~body
      |> function
      | Ok message -> Letters.send ~config ~sender ~recipients ~message
      | Error msg -> raise (Sihl.Contract.Email.Exception msg)
    ;;
  end

  let send database_label email =
    Logs.info (fun m -> m "Send email to %s" email.Sihl_email.recipient);
    let%lwt () = intercept (Smtp.send' database_label) email in
    Logs.info (fun m -> m "Email sent");
    Lwt.return_unit
  ;;

  let bulk_send _ _ =
    failwith
      "Bulk sending with the SMTP backend not supported, please use sihl-queue"
  ;;

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
end
