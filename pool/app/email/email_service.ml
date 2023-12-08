module SmtpAuth = Entity.SmtpAuth
module Queue = Sihl_queue.MariaDb

module Cache = struct
  open Hashtbl

  let tbl : (SmtpAuth.Id.t, SmtpAuth.Write.t) t = create 5
  let find_by_id id = find_opt tbl id

  let find_default () =
    tbl
    |> to_seq_values
    |> Seq.find (fun { SmtpAuth.Write.default; _ } ->
      SmtpAuth.Default.value default)
  ;;

  let add ({ SmtpAuth.Write.id; _ } as m) = replace tbl id m
  let update = add
  let clear () = clear tbl
end

let src = Logs.Src.create "pool_tenant.service"
let tags = Pool_database.(Logger.Tags.create root)

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

let intercept_prepare ({ Entity.email; _ } as job) =
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
    Ok Entity.{ job with email }
  | false, None ->
    Error
      (Pool_common.Message.EmailInterceptionError
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
  if Pool_database.is_root database_label
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

  let prepare
    database_label
    ?smtp_auth_id
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
      let cached =
        match smtp_auth_id with
        | Some id -> Cache.find_by_id id
        | None -> Cache.find_default ()
      in
      match cached with
      | None ->
        let open Repo.Smtp in
        let%lwt auth =
          smtp_auth_id
          |> CCOption.map_or
               ~default:(find_full_default database_label)
               (fun id -> find_full database_label id)
          >|- with_log_error
                ~src
                ~tags:(Pool_database.Logger.Tags.create database_label)
          ||> get_or_failwith
        in
        let () = Cache.add auth in
        Lwt.return auth
      | Some auth -> Lwt.return auth
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
      let%lwt default_sender_of_pool =
        default_sender_of_pool database_label ||> Pool_user.EmailAddress.value
      in
      [ username; Some default_sender_of_pool ]
      |> CCList.filter (CCOption.map_or ~default:false valid_email)
      |> CCOption.choice
      |> CCOption.get_or ~default:sender
      |> Lwt.return
    in
    Lwt.return { sender; reply_to; recipients; subject; body; config }
  ;;

  let send ?smtp_auth_id database_label email =
    let%lwt { sender; reply_to; recipients; subject; body; config } =
      prepare ?smtp_auth_id database_label email
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

let send ?smtp_auth_id database_label =
  intercept_send (Smtp.send ?smtp_auth_id database_label)
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
    let handle ?ctx { Entity.email; smtp_auth_id } =
      let database_label =
        let open CCOption in
        ctx
        >>= CCList.assoc_opt ~eq:( = ) "pool"
        >|= Pool_database.Label.create %> Pool_common.Utils.get_or_failwith
        |> get_exn_or "Invalid context passed!"
      in
      Lwt.catch
        (fun () -> send ?smtp_auth_id database_label email ||> CCResult.return)
        (Printexc.to_string %> Lwt.return_error)
    in
    let encode = Entity.yojson_of_job %> Yojson.Safe.to_string in
    let decode email =
      try Ok (email |> Yojson.Safe.from_string |> Entity.job_of_yojson) with
      | Yojson.Json_error msg ->
        Logs.err ~src (fun m ->
          m
            ~tags
            "Serialized email string was NULL, can not deserialize email. \
             Please fix the string manually and reset the job instance. Error: \
             %s"
            msg);
        Error "Invalid serialized email string received"
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

let dispatch database_label (email, smtp_auth_id) =
  Logs.debug ~src (fun m ->
    m
      ~tags:(Pool_database.Logger.Tags.create database_label)
      "Dispatch email to %s"
      email.Sihl_email.recipient);
  Queue.dispatch
    ~ctx:(Pool_database.to_ctx database_label)
    (Entity.create_job email smtp_auth_id
     |> intercept_prepare
     |> Pool_common.Utils.get_or_failwith)
    Job.send
;;

let dispatch_all database_label jobs =
  let recipients, jobs =
    jobs
    |> CCList.fold_left
         (fun (recipients, jobs) (email, smtp_auth_id) ->
           let job =
             Entity.create_job email smtp_auth_id
             |> intercept_prepare
             |> Pool_common.Utils.get_or_failwith
           in
           email.Sihl_email.recipient :: recipients, job :: jobs)
         ([], [])
  in
  Logs.debug ~src (fun m ->
    m
      ~tags:(Pool_database.Logger.Tags.create database_label)
      "Dispatch email to %s"
      ([%show: string list] recipients));
  Queue.dispatch_all ~ctx:(Pool_database.to_ctx database_label) jobs Job.send
;;
