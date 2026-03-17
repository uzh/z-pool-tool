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
let tags = Database.(Logger.Tags.create Pool.Root.label)

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
  CCOption.get_or ~default:false (Sihl.Configuration.read_bool "EMAIL_BYPASS_INTERCEPT")
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
         "Sending email intercepted! As no redirect email is specified it/they wont be \
          sent. Please define environment variable 'TEST_EMAIL'.")
;;

let intercept_send sender email =
  let () = if console () then print email else () in
  match Sihl.Configuration.is_production (), bypass () with
  | true, _ | _, true -> sender email
  | false, false ->
    Logs.info ~src (fun m ->
      m ~tags "Sending email intercepted (non production environment -> DevInbox).");
    email |> DevInbox.add_to_inbox |> Lwt.return
;;

let default_sender_of_pool database_label =
  let open Settings in
  let open Utils.Lwt_result.Infix in
  if Database.Pool.is_root database_label
  then
    Sihl.Configuration.read_string "SMTP_SENDER"
    |> CCOption.get_exn_or "Undefined 'SMTP_SENDER'"
    |> Pool_user.EmailAddress.of_string
    |> Lwt.return
  else
    find_contact_email database_label
    ||> fun sender -> sender |> ContactEmail.value |> Pool_user.EmailAddress.of_string
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
    let%lwt { SmtpAuth.Write.server; port; username; password; mechanism; protocol; _ } =
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
        match Database.Pool.is_root database_label with
        | true -> default_sender_of_pool |> Pool_user.EmailAddress.value |> Lwt.return
        | false ->
          Settings.(find_contact_email database_label |> Lwt.map ContactEmail.value)
      in
      let recipient = test_email |> Pool_user.EmailAddress.value in
      let subject = "Test email" in
      let text = "This is a test" in
      let html = Some text in
      Lwt.return
        Sihl.Contract.Email.{ sender; recipient; subject; text; html; cc = []; bcc = [] }
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

  let is_system_email_template
        (database_label : Database.Label.t)
        (template : Pool_common.MessageTemplateLabel.t)
    : bool Lwt.t
    =
    let open Utils.Lwt_result.Infix in
    Settings.find_system_email_templates database_label
    ||> (CCList.mem ~eq:Pool_common.MessageTemplateLabel.equal) template
  ;;

  let resolve_system_smtp_auth_id database_label =
    let%lwt auth = Repo_sql.Smtp.find_full_system_or_default_opt database_label in
    auth |> CCOption.map SmtpAuth.Write.(fun { id; _ } -> id) |> Lwt.return
  ;;

  let encode = yojson_of_t %> Yojson.Safe.to_string

  let decode str =
    try Ok (str |> Yojson.Safe.from_string |> t_of_yojson) with
    | _ ->
      Logs.err ~src (fun m ->
        m
          ~tags
          "Serialized email string was NULL, can not deserialize email. Please fix the \
           string manually and reset the job instance. Error: %s"
          str);
      Error Pool_message.(Error.Invalid Field.Input)
  ;;

  let intercept_prepare_of_event = intercept_prepare %> Pool_common.Utils.get_or_failwith

  let show_recipient =
    Pool_queue.Instance.input
    %> decode
    %> CCResult.map_or ~default:"error" (email %> fun x -> x.Sihl_email.recipient)
  ;;

  let send =
    let open Utils.Lwt_result.Infix in
    let increment_smtp_bounce = Repo_sql.Contact.increment_smtp_bounce in
    let reset_smtp_bounce = Repo_sql.Contact.reset_smtp_bounce in
    let is_recipient_not_found msg =
      (*
         Error Documentation:
         * https://learn.microsoft.com/en-us/troubleshoot/exchange/email-delivery/ndr/recipientnotfound-ndr
      *)
      CCString.find ~sub:"550" msg >= 0
      && CCString.find ~sub:"RESOLVER.ADR.RecipientNotFound" msg >= 0
    in
    let handle ?id:_ label (job : t) =
      let email_data = email job in
      let smtp_auth_id = smtp_auth_id job in
      Lwt.catch
        (fun () ->
           let%lwt result =
             Smtp.send ?smtp_auth_id label email_data ||> CCResult.return
           in
           let%lwt () =
             reset_smtp_bounce
               label
               (Pool_user.EmailAddress.of_string email_data.Sihl_email.recipient)
           in
           Lwt.return result)
        (fun exn ->
           match Printexc.to_string exn with
           | msg when is_recipient_not_found msg ->
             let recipient = email_data.Sihl_email.recipient in
             Logs.info ~src (fun m ->
               m
                 ~tags:(Database.Logger.Tags.create label)
                 "SMTP 550 bounce for %s — incrementing smtp_bounces_count"
                 recipient);
             let%lwt () =
               increment_smtp_bounce label (Pool_user.EmailAddress.of_string recipient)
             in
             Lwt.return_error (Pool_message.Error.SmtpRecipientNotFound recipient)
           | msg -> Lwt.return_error (Pool_message.Error.nothandled msg))
    in
    Pool_queue.(
      Job.create
        ~max_tries:10
        ~retry_delay:(Sihl.Time.Span.hours 1)
        handle
        encode
        decode
        JobName.SendEmail)
  ;;
end

let dispatch
      ?id
      ?new_email_address
      ?new_smtp_auth_id
      ?message_template
      ?(job_ctx = Pool_queue.job_ctx_create [])
      database_label
      ({ Entity.Job.email; _ } as job)
  =
  let tags = Database.Logger.Tags.create database_label in
  Logs.debug ~src (fun m -> m ~tags "Dispatch email to %s" email.Sihl_email.recipient);
  let%lwt resolved =
    match new_smtp_auth_id, Job.smtp_auth_id job, message_template with
    | Some id, _, _ -> Lwt.return_some id
    | None, Some _, _ | None, None, None -> Lwt.return_none
    | None, None, Some template ->
      (match%lwt Job.is_system_email_template database_label template with
       | true -> Job.resolve_system_smtp_auth_id database_label
       | false -> Lwt.return_none)
  in
  let job = job |> Job.update ?new_email_address ?new_smtp_auth_id:resolved in
  Pool_queue.dispatch
    ?id
    ?message_template
    ~job_ctx
    database_label
    (job |> Job.intercept_prepare_of_event)
    Job.send
;;

let dispatch_all database_label jobs =
  let system_template_cache = Hashtbl.create 4 in
  let resolved_system_smtp_auth_id = ref None in
  let%lwt recipients, jobs =
    Lwt_list.fold_left_s
      (fun (recipients, jobs)
        (id, ({ Entity.Job.email; _ } as job), message_template, mappings) ->
         let%lwt resolved_new_smtp_auth_id =
           match Job.smtp_auth_id job, message_template with
           | Some _, _ -> Lwt.return_none
           | None, Some template ->
             let%lwt is_system_template =
               Utils.Lwt_cache.cached system_template_cache template (fun () ->
                 Job.is_system_email_template database_label template)
             in
             (match is_system_template with
              | true ->
                Utils.Lwt_cache.once resolved_system_smtp_auth_id (fun () ->
                  Job.resolve_system_smtp_auth_id database_label)
              | false -> Lwt.return_none)
           | None, _ -> Lwt.return_none
         in
         let job = job |> Job.update ?new_smtp_auth_id:resolved_new_smtp_auth_id in
         Lwt.return
           ( email.Sihl_email.recipient :: recipients
           , ( id
             , job |> Job.intercept_prepare_of_event
             , message_template
             , CCOption.get_or ~default:(Pool_queue.job_ctx_create []) mappings )
             :: jobs ))
      ([], [])
      jobs
  in
  Logs.debug ~src (fun m ->
    m
      ~tags:(Database.Logger.Tags.create database_label)
      "Dispatch email to %s"
      ([%show: string list] recipients));
  Pool_queue.dispatch_all database_label jobs Job.send
;;
