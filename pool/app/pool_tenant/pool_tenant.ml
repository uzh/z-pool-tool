include Entity
include Event

module Url = struct
  include Entity.Url

  let of_pool = Repo.Url.of_pool
end

let to_ctx pool = [ "pool", Database.Label.value pool ]
let find = Repo.find Database.root
let find_full = Repo.find_full Database.root
let find_by_label = Repo.find_by_label Database.root
let find_all = Repo.find_all Database.root
let find_databases = Repo.find_databases Database.root

let find_styles db_pool =
  let open Utils.Lwt_result.Infix in
  Repo.find_by_label Database.root db_pool >|+ fun { styles; _ } -> styles
;;

type handle_list_recruiters = unit -> Sihl_user.t list Lwt.t
type handle_list_tenants = unit -> t list Lwt.t

module Selection = struct
  include Selection

  let find_all = Repo.find_selectable Database.root
end

module LogoMapping = struct
  include LogoMapping
end

module Guard = struct
  include Entity_guard
end

module Service = struct
  module Queue = Sihl_queue.MariaDb

  module Email = struct
    let dev_inbox : Sihl.Contract.Email.t list ref = ref []

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
          (Option.value ~default:"<None>" email.html))
    ;;

    let should_intercept () =
      let is_production = Sihl.Configuration.is_production () in
      let bypass =
        Option.value
          ~default:false
          (Sihl.Configuration.read_bool "EMAIL_BYPASS_INTERCEPT")
      in
      match is_production, bypass with
      | false, true -> false
      | false, false -> true
      | true, _ -> false
    ;;

    let intercept sender email =
      let is_development = Sihl.Configuration.is_development () in
      let console =
        Option.value
          ~default:is_development
          (Sihl.Configuration.read_bool "EMAIL_CONSOLE")
      in
      let () = if console then print email else () in
      if should_intercept ()
      then Lwt.return (DevInbox.add_to_inbox email)
      else sender email
    ;;

    module Smtp = struct
      include DevInbox

      let send'
        database_label
        { Sihl.Contract.Email.sender; recipient; subject; text; html; cc; bcc }
        =
        let open Utils.Lwt_result.Infix in
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
          Repo.find_smtp_by_label database_label
          >|- with_log_error
          ||> get_or_failwith
        in
        let username = SmtpAuth.(Username.value config.Write.username) in
        let password = SmtpAuth.(Password.value config.Write.password) in
        let hostname = SmtpAuth.(Server.value config.Write.server) in
        let port = Some SmtpAuth.(Port.value config.Write.port) in
        let mechanism =
          SmtpAuth.(
            AuthenticationMethod.value config.Write.authentication_method)
          |> function
          | "LOGIN" -> Sendmail.LOGIN
          | "PLAIN" -> Sendmail.PLAIN
          | _ ->
            raise
              (Sihl.Contract.Email.Exception
                 "SMTP Authentication mechanism not set")
        in
        let with_starttls =
          CCString.uppercase_ascii
            SmtpAuth.(Protocol.value config.Write.protocol)
          |> CCString.equal "STARTTLS"
        in
        let config =
          Letters.Config.create
            ~mechanism
            ~username
            ~password
            ~hostname
            ~with_starttls
            ()
          |> Letters.Config.set_port port
        in
        Letters.build_email ~from:sender ~recipients ~subject ~body
        |> function
        | Ok message -> Letters.send ~config ~sender ~recipients ~message
        | Error msg -> raise (Sihl.Contract.Email.Exception msg)
      ;;

      let send ?ctx email =
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
        intercept (send' database_label) email
      ;;

      let bulk_send ?ctx:_ _ =
        failwith
          "Bulk sending with the SMTP backend not supported, please use \
           sihl-queue"
      ;;

      let start () =
        (* Make sure that configuration is valid *)
        if Sihl.Configuration.is_production ()
        then Sihl.Configuration.require Repo.RepoEntity.SmtpAuth.Write.schema
        else ();
        (* If mail is intercepted, don't punish user for not providing SMTP
           credentials *)
        if should_intercept ()
        then ()
        else Sihl.Configuration.require Repo.RepoEntity.SmtpAuth.Write.schema;
        Lwt.return ()
      ;;

      let stop () = Lwt.return ()

      let lifecycle =
        Sihl.Container.create_lifecycle Sihl.Contract.Email.name ~start ~stop
      ;;

      let register () =
        let configuration =
          Sihl.Configuration.make
            ~schema:Repo.RepoEntity.SmtpAuth.Write.schema
            ()
        in
        Sihl.Container.Service.create ~configuration lifecycle
      ;;
    end

    (* NOTE: The mail intercepter will most likely be implemented within SIHL
       4.0.0 *)
    include Sihl_email.Queued (Queue) (Smtp)

    module Job = struct
      let send =
        let open CCFun in
        let open Utils.Lwt_result.Infix in
        let handle ?ctx email =
          Lwt.catch
            (fun () -> Smtp.send ?ctx email ||> CCResult.pure)
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

    let redirected_email
      new_recipient
      (Sihl_email.{ recipient; subject; _ } as email)
      =
      let subject =
        Format.asprintf "[Pool Tool] %s (original to: %s)" subject recipient
      in
      Sihl_email.{ email with subject; recipient = new_recipient }
    ;;

    let handle
      ?ctx
      ?(without_email_fcn = fun ?ctx:_ _ -> Lwt.return_unit)
      production_fcn
      else_fcn
      email
      =
      match Sihl.Configuration.is_production () with
      | true -> production_fcn ?ctx email
      | false ->
        (match Sihl.Configuration.read_string "TEST_EMAIL" with
         | None ->
           Logs.err (fun m ->
             m
               "%s"
               "Sending email intercepted! As no redirect email is specified \
                it/they wont be sent. Please define environment variable \
                'TEST_EMAIL'.");
           without_email_fcn ?ctx email
         | Some new_recipient ->
           Logs.info (fun m ->
             m
               "Sending email intercepted. Sending email(s) to new recipient \
                ('%s')"
               new_recipient);
           else_fcn ?ctx email new_recipient)
    ;;

    let set_email_sender ?sender (email : Sihl_email.t) =
      match sender with
      | Some sender -> Sihl_email.{ email with sender }
      | None -> email
    ;;

    let send ?sender ?ctx email =
      Logs.info (fun m -> m "Send email to %s" email.Sihl_email.recipient);
      let email = set_email_sender ?sender email in
      let%lwt () =
        handle
          ?ctx
          send
          (fun ?ctx email new_recipient ->
            email |> redirected_email new_recipient |> send ?ctx)
          email
      in
      Logs.info (fun m -> m "Email sent");
      Lwt.return ()
    ;;

    let bulk_send ?sender ?ctx emails =
      let emails = CCList.map (set_email_sender ?sender) emails in
      Logs.info (fun m -> m "Send %d emails" (CCList.length emails));
      let%lwt () =
        handle
          ?ctx
          bulk_send
          (fun ?ctx emails new_recipient ->
            emails
            |> CCList.map (redirected_email new_recipient)
            |> bulk_send ?ctx)
          emails
      in
      Logs.info (fun m -> m "%d emails sent" (CCList.length emails));
      Lwt.return ()
    ;;
  end
end
