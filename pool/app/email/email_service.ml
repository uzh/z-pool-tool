module SmtpAuth = Entity.SmtpAuth
module AccountMap = CCMap.Make (Pool_database.Label)
module Queue = Sihl_queue.MariaDb

let src = Logs.Src.create "pool_tenant.service"
let tags = Pool_database.(Logger.Tags.create root)
let accounts : SmtpAuth.Write.t AccountMap.t ref = ref AccountMap.empty

let remove_from_cache label =
  accounts := AccountMap.remove label !accounts;
  ()
;;

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

let intercept_prepare email =
  let () = if console () then print ~log_level:Logs.Info email else () in
  match Sihl.Configuration.is_production (), intercept_email_address () with
  | true, _ -> Ok email
  | false, Some new_recipient ->
    Logs.info ~src (fun m ->
      m
        ~tags
        "Sending email intercepted. Sending email to new recipient ('%s')"
        new_recipient);
    email |> redirected_email new_recipient |> CCResult.return
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
    Logs.info ~src (fun m ->
      m
        ~tags
        "Sending email intercepted (non production environment -> DevInbox).");
    email |> DevInbox.add_to_inbox |> Lwt.return
;;

let sender_of_pool database_label =
  let open Settings in
  if Pool_database.is_root database_label
  then
    Sihl.Configuration.read_string "SMTP_SENDER"
    |> CCOption.get_exn_or "Undefined 'SMTP_SENDER'"
    |> ContactEmail.of_string
    |> Lwt.return
  else find_contact_email database_label
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
      | Not_found ->
        let%lwt from_repo =
          Repo.Smtp.find_full_by_label database_label
          >|- with_log_error
                ~src
                ~tags:(Pool_database.Logger.Tags.create database_label)
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
    let reply_to = sender in
    let%lwt sender =
      let valid_email =
        let open Pool_user.EmailAddress in
        of_string %> validate_characters %> CCResult.is_ok
      in
      let%lwt sender_of_pool =
        sender_of_pool database_label ||> Settings.ContactEmail.value
      in
      [ username; Some sender_of_pool ]
      |> CCList.filter (CCOption.map_or ~default:false valid_email)
      |> CCOption.choice
      |> CCOption.get_or ~default:sender
      |> Lwt.return
    in
    Lwt.return { sender; reply_to; recipients; subject; body; config }
  ;;

  let send database_label email =
    let%lwt { sender; reply_to; recipients; subject; body; config } =
      prepare database_label email
    in
    Letters.create_email ~reply_to ~from:sender ~recipients ~subject ~body ()
    |> function
    | Ok message ->
      Logs.info ~src (fun m ->
        m
          ~tags:(Pool_database.Logger.Tags.create database_label)
          "Send email as %s to %s"
          sender
          email.Sihl_email.recipient);
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
        >|= Pool_database.Label.create %> Pool_common.Utils.get_or_failwith
        |> get_exn_or "Invalid context passed!"
      in
      Lwt.catch
        (fun () -> send database_label email ||> CCResult.return)
        (Printexc.to_string %> Lwt.return_error)
    in
    let encode = Sihl.Contract.Email.to_yojson %> Yojson.Safe.to_string in
    let decode email =
      let open CCResult.Infix in
      (try Ok (Yojson.Safe.from_string email) with
       | Yojson.Json_error msg ->
         Logs.err ~src (fun m ->
           m
             ~tags
             "Serialized email string was NULL, can not deserialize email. \
              Please fix the string manually and reset the job instance. \
              Error: %s"
             msg);
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
  Logs.debug ~src (fun m ->
    m
      ~tags:(Pool_database.Logger.Tags.create database_label)
      "Dispatch email to %s"
      email.Sihl_email.recipient);
  Queue.dispatch
    ~ctx:(Pool_database.to_ctx database_label)
    (email |> intercept_prepare |> CCResult.get_or_failwith)
    Job.send
;;

let dispatch_all database_label emails =
  let recipients = CCList.map (fun m -> m.Sihl_email.recipient) emails in
  Logs.debug ~src (fun m ->
    m
      ~tags:(Pool_database.Logger.Tags.create database_label)
      "Dispatch email to %s"
      ([%show: string list] recipients));
  Queue.dispatch_all ~ctx:(Pool_database.to_ctx database_label) emails Job.send
;;
