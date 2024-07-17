open CCFun
module SmtpAuth = Entity.SmtpAuth

module Cache = struct
  open Hashtbl

  let tbl : (SmtpAuth.Id.t * Database.Label.t, SmtpAuth.Write.t) t = create 5
  let find_by_id datbase_label id = find_opt tbl (id, datbase_label)

  let find_default pool =
    tbl
    |> to_seq
    |> Seq.find (fun ((_, database_label), { SmtpAuth.Write.default; _ }) ->
      SmtpAuth.Default.value default && Database.Label.equal pool database_label)
    |> CCOption.map snd
  ;;

  let add database_label ({ SmtpAuth.Write.id; _ } as m) =
    replace tbl (id, database_label) m
  ;;

  let update = add
  let clear () = clear tbl
end

let src = Logs.Src.create "pool_tenant.service"
let tags = Database.(Logger.Tags.create root)

module DevInbox = struct
  let dev_inbox : Sihl.Contract.Email.t list ref = ref []
  let inbox () = !dev_inbox
  let add_to_inbox email = dev_inbox := email :: !dev_inbox
  let clear_inbox () = dev_inbox := []
end

let print ?(tags = tags) ?(log_level = Logs.Debug) email =
  let open Sihl.Contract.Email in
  Logs.msg ~src log_level (fun m ->
    m
      ~tags
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

let intercept_prepare ({ Entity.Job.email; _ } as job) =
  let () = if console () then print ~log_level:Logs.Info email else () in
  match Sihl.Configuration.is_production (), intercept_email_address () with
  | true, _ -> Ok job
  | false, Some new_recipient ->
    Logs.info ~src (fun m ->
      m
        ~tags
        "Sending email intercepted. Sending email to new recipient ('%s')"
        new_recipient);
    let email = email |> redirected_email new_recipient in
    Ok { job with Entity.Job.email }
  | false, None ->
    Error
      (Pool_message.Error.EmailInterceptionError
         "Sending email intercepted! As no redirect email is specified it/they \
          wont be sent. Please define environment variable 'TEST_EMAIL'.")
;;

let intercept_send sender email =
  let () = if console () then print email else () in
  match Sihl.Configuration.is_production (), bypass () with
  | true, _ | _, true -> sender email
  | false, false ->
    Logs.info ~src (fun m ->
      m
        ~tags
        "Sending email intercepted (non production environment -> DevInbox).");
    email |> DevInbox.add_to_inbox |> Lwt.return
;;

let default_sender_of_pool database_label =
  let open Settings in
  let open Utils.Lwt_result.Infix in
  if Database.is_root database_label
  then
    Sihl.Configuration.read_string "SMTP_SENDER"
    |> CCOption.get_exn_or "Undefined 'SMTP_SENDER'"
    |> Pool_user.EmailAddress.of_string
    |> Lwt.return
  else
    find_contact_email database_label
    ||> fun sender ->
    sender |> ContactEmail.value |> Pool_user.EmailAddress.of_string
;;

module Smtp = struct
  include DevInbox

  type prepared =
    { sender : string
    ; reply_to : string
    ; recipients : Letters.recipient list
    ; subject : string
    ; body : Letters.body
    ; config : Letters.Config.t
    }

  let email_from_smtp_auth
    username
    password
    server
    port
    mechanism
    protocol
    default_sender_of_pool
    { Sihl.Contract.Email.sender; recipient; subject; text; html; cc; bcc }
    =
    let open CCFun.Infix in
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
    let username = CCOption.(username >|= SmtpAuth.Username.value) in
    let password = CCOption.(password >|= SmtpAuth.Password.value) in
    let hostname = SmtpAuth.Server.value server in
    let port = Some (SmtpAuth.Port.value port) in
    let mechanism =
      SmtpAuth.validate_mechanism mechanism username password
      |> CCResult.map_err Pool_common.(Utils.error_to_string Language.En)
      |> CCResult.get_or_failwith
      |> SmtpAuth.Mechanism.to_sendmail
    in
    let with_starttls = SmtpAuth.Protocol.(equal STARTTLS) protocol in
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
    let reply_to = sender in
    let%lwt sender =
      let valid_email =
        let open Pool_user.EmailAddress in
        of_string %> validate_characters %> CCResult.is_ok
      in
      [ username; Some (Pool_user.EmailAddress.value default_sender_of_pool) ]
      |> CCList.filter (CCOption.map_or ~default:false valid_email)
      |> CCOption.choice
      |> CCOption.get_or ~default:sender
      |> Lwt.return
    in
    Lwt.return { sender; reply_to; recipients; subject; body; config }
  ;;

  let prepare database_label ?smtp_auth_id email =
    let open Utils.Lwt_result.Infix in
    let%lwt { SmtpAuth.Write.server
            ; port
            ; username
            ; password
            ; mechanism
            ; protocol
            ; _
            }
      =
      let open Pool_common.Utils in
      let cached =
        match smtp_auth_id with
        | Some id -> Cache.find_by_id database_label id
        | None -> Cache.find_default database_label
      in
      match cached with
      | None ->
        let open Repo.Smtp in
        let%lwt auth =
          let tags = Database.Logger.Tags.create database_label in
          smtp_auth_id
          |> CCOption.map_or
               ~default:(find_full_default database_label)
               (find_full database_label)
          >|- with_log_error ~src ~tags
          ||> get_or_failwith
        in
        let () = Cache.add database_label auth in
        Lwt.return auth
      | Some auth -> Lwt.return auth
    in
    let%lwt default_sender_of_pool = default_sender_of_pool database_label in
    email_from_smtp_auth
      username
      password
      server
      port
      mechanism
      protocol
      default_sender_of_pool
      email
  ;;

  let prepare_test_email
    database_label
    { SmtpAuth.Write.server; port; username; password; mechanism; protocol; _ }
    test_email
    =
    let%lwt default_sender_of_pool = default_sender_of_pool database_label in
    let%lwt test_email =
      let%lwt sender =
        Settings.(
          find_contact_email database_label |> Lwt.map ContactEmail.value)
      in
      let recipient = test_email |> Pool_user.EmailAddress.value in
      let subject = "Test email" in
      let text = "This is a test" in
      let html = Some text in
      Lwt.return
        Sihl.Contract.Email.
          { sender; recipient; subject; text; html; cc = []; bcc = [] }
    in
    email_from_smtp_auth
      username
      password
      server
      port
      mechanism
      protocol
      default_sender_of_pool
      test_email
  ;;

  let send ?smtp_auth_id database_label email =
    let%lwt { sender; reply_to; recipients; subject; body; config } =
      prepare ?smtp_auth_id database_label email
    in
    Letters.create_email ~reply_to ~from:sender ~recipients ~subject ~body ()
    |> function
    | Ok message ->
      Logs.debug ~src (fun m ->
        m
          ~tags:(Database.Logger.Tags.create database_label)
          "Send email as %s to %s"
          sender
          email.Sihl_email.recipient);
      Letters.send ~config ~sender ~recipients ~message
    | Error msg -> raise (Sihl.Contract.Email.Exception msg)
  ;;
end

let test_smtp_config database_label config test_email_address =
  let open Utils.Lwt_result.Infix in
  let open Smtp in
  let%lwt { sender; reply_to; recipients; subject; body; config } =
    prepare_test_email database_label config test_email_address
  in
  let send_mail () =
    let message =
      Letters.create_email ~reply_to ~from:sender ~recipients ~subject ~body ()
      |> CCResult.get_exn
    in
    Letters.send ~config ~sender ~recipients ~message
  in
  Lwt_result.catch send_mail
  >|- fun exn -> Pool_message.Error.SmtpException (Printexc.to_string exn)
;;

let send ?smtp_auth_id database_label =
  intercept_send (Smtp.send ?smtp_auth_id database_label)
;;

let start () = Lwt.return_unit
let stop () = Lwt.return_unit

let lifecycle =
  Sihl.Container.create_lifecycle
    Sihl.Contract.Email.name
    ~dependencies:(fun () -> [ Pool_database.lifecycle ])
    ~start
    ~stop
;;

let register () =
  let configuration = Sihl.Configuration.make () in
  Sihl.Container.Service.create ~configuration lifecycle
;;

module Job = struct
  include Entity.Job

  let encode = yojson_of_t %> Yojson.Safe.to_string

  let decode str =
    try Ok (str |> Yojson.Safe.from_string |> t_of_yojson) with
    | _ ->
      Logs.err ~src (fun m ->
        m
          ~tags
          "Serialized email string was NULL, can not deserialize email. Please \
           fix the string manually and reset the job instance. Error: %s"
          str);
      Error Pool_message.(Error.Invalid Field.Input)
  ;;

  let intercept_prepare_of_event =
    intercept_prepare %> Pool_common.Utils.get_or_failwith
  ;;

  let send =
    let open CCFun in
    let open Utils.Lwt_result.Infix in
    let open Pool_queue in
    let handle label ({ email; smtp_auth_id } : t) =
      Lwt.catch
        (fun () -> send ?smtp_auth_id label email ||> CCResult.return)
        (Printexc.to_string %> Pool_message.Error.nothandled %> Lwt.return_error)
    in
    Job.create
      ~max_tries:10
      ~retry_delay:(Sihl.Time.Span.hours 1)
      handle
      encode
      decode
      JobName.SendEmail
  ;;
end

let dispatch
  ?id
  ?new_email_address
  ?new_smtp_auth_id
  ?message_template
  ?(mappings = Pool_queue.mappings_create [])
  database_label
  ({ Entity.Job.email; _ } as job)
  =
  let tags = Database.Logger.Tags.create database_label in
  Logs.debug ~src (fun m ->
    m ~tags "Dispatch email to %s" email.Sihl_email.recipient);
  let job = job |> Job.update ?new_email_address ?new_smtp_auth_id in
  Pool_queue.dispatch
    ?id
    ?message_template
    ~mappings
    database_label
    (job |> Job.intercept_prepare_of_event)
    Job.send
;;

let dispatch_all database_label jobs =
  let recipients, jobs =
    jobs
    |> CCList.fold_left
         (fun (recipients, jobs)
           (id, ({ Entity.Job.email; _ } as job), message_template, mappings) ->
           ( email.Sihl_email.recipient :: recipients
           , ( id
             , job |> Job.intercept_prepare_of_event
             , message_template
             , CCOption.get_or ~default:(Pool_queue.mappings_create []) mappings
             )
             :: jobs ))
         ([], [])
  in
  Logs.debug ~src (fun m ->
    m
      ~tags:(Database.Logger.Tags.create database_label)
      "Dispatch email to %s"
      ([%show: string list] recipients));
  Pool_queue.dispatch_all database_label jobs Job.send
;;
