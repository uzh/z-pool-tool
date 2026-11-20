open Pool_message
module Command = Cqrs_command.Signup_code_command

let get_exn = Test_utils.get_or_failwith
let testable_signup_code = Signup_code.(Alcotest.testable pp equal)
let testable_webhook = Signup_code.Webhook.(Alcotest.testable pp equal)

module Data = struct
  open Signup_code

  let code_string = "newsletter"
  let signup_started_url_string = "https://example.com/hooks/signup-started"
  let signup_completed_url_string = "https://example.com/hooks/signup-completed"
  let code = Code.of_string code_string
  let signup_started_url = WebhookUrl.of_string signup_started_url_string
  let signup_completed_url = WebhookUrl.of_string signup_completed_url_string
  let payload = Payload.of_string "click=42&campaign=autumn"

  let unique_code prefix =
    Format.asprintf "%s-%s" prefix Id.(create () |> value) |> Code.of_string
  ;;

  let urlencoded ?signup_started ?signup_completed () =
    let webhook field = CCOption.map_or ~default:[] (fun url -> [ field, [ url ] ]) in
    [ Field.(show Code), [ code_string ] ]
    @ webhook Field.(show SignUpStartedWebhookUrl) signup_started
    @ webhook Field.(show SignUpCompletedWebhookUrl) signup_completed
  ;;

  let with_webhooks ?id () =
    create
      ?id
      ~signup_started_webhook_url:signup_started_url
      ~signup_completed_webhook_url:signup_completed_url
      code
  ;;
end

let create_with_webhooks () =
  let open Signup_code in
  let id = Id.create () in
  let events =
    let open CCResult in
    Data.urlencoded
      ~signup_started:Data.signup_started_url_string
      ~signup_completed:Data.signup_completed_url_string
      ()
    |> Command.Create.decode
    >>= Command.Create.handle ~id None
  in
  let expected = Ok [ Created (Data.with_webhooks ~id ()) |> Pool_event.signupcode ] in
  Test_utils.check_result ~msg:"Create signup code with webhooks" expected events
;;

let create_without_webhooks () =
  let open Signup_code in
  let id = Id.create () in
  let events =
    let open CCResult in
    Data.urlencoded () |> Command.Create.decode >>= Command.Create.handle ~id None
  in
  let expected = Ok [ Created (create ~id Data.code) |> Pool_event.signupcode ] in
  Test_utils.check_result ~msg:"Create signup code without webhooks" expected events
;;

let create_with_invalid_webhook_url () =
  let events =
    let open CCResult in
    Data.urlencoded ~signup_started:"ftp://example.com/hook" ()
    |> Command.Create.decode
    >>= Command.Create.handle None
  in
  let expected =
    Error Error.(Conformist [ Field.SignUpStartedWebhookUrl, Invalid Field.Url ])
  in
  Test_utils.check_result ~msg:"Reject non http(s) webhook url" expected events
;;

let create_existing_code () =
  let events =
    let open CCResult in
    Data.urlencoded ()
    |> Command.Create.decode
    >>= Command.Create.handle (Some (Signup_code.create Data.code))
  in
  let expected = Error (Error.AlreadyExisting Field.Code) in
  Test_utils.check_result ~msg:"Reject existing code" expected events
;;

let update_webhooks () =
  let open Signup_code in
  let signup_code = create Data.code in
  let events =
    let open CCResult in
    [ Field.(show SignUpCompletedWebhookUrl), [ Data.signup_completed_url_string ] ]
    |> Command.Update.decode
    >>= Command.Update.handle signup_code
  in
  let expected =
    Ok
      [ Updated
          { signup_code with
            signup_completed_webhook_url = Some Data.signup_completed_url
          }
        |> Pool_event.signupcode
      ]
  in
  Test_utils.check_result ~msg:"Set a webhook on an existing code" expected events
;;

let remove_webhooks () =
  let open Signup_code in
  let signup_code = Data.with_webhooks () in
  let events =
    let open CCResult in
    [] |> Command.Update.decode >>= Command.Update.handle signup_code
  in
  let expected =
    Ok
      [ Updated
          { signup_code with
            signup_started_webhook_url = None
          ; signup_completed_webhook_url = None
          }
        |> Pool_event.signupcode
      ]
  in
  Test_utils.check_result ~msg:"Remove webhooks from a code" expected events
;;

let webhook_without_url () =
  let open Signup_code in
  let signup_code =
    create ~signup_started_webhook_url:Data.signup_started_url Data.code
  in
  Alcotest.(check (option testable_webhook))
    "no webhook when the url of the trigger is missing"
    None
    (Signup_code.webhook
       Webhook.Trigger.SignUpCompleted
       ~payload:Data.payload
       signup_code)
;;

let webhook_with_url () =
  let open Signup_code in
  let signup_code = Data.with_webhooks () in
  let check msg trigger url payload =
    Alcotest.(check (option testable_webhook))
      msg
      (Some { Webhook.url; trigger; code = Data.code; payload })
      (Signup_code.webhook trigger ?payload signup_code)
  in
  let open Webhook.Trigger in
  check "started with payload" SignUpStarted Data.signup_started_url (Some Data.payload);
  check "completed without payload" SignUpCompleted Data.signup_completed_url None
;;

let webhook_request_uri () =
  let open Signup_code in
  let uri ?(url = Data.signup_started_url) payload =
    { Webhook.url; trigger = Webhook.Trigger.SignUpStarted; code = Data.code; payload }
    |> Webhook.request_uri
    |> Uri.to_string
  in
  let check = Alcotest.(check string) in
  check
    "with payload"
    "https://example.com/hooks/signup-started?signup_code=newsletter&payload=click=42%26campaign=autumn"
    (uri (Some Data.payload));
  check
    "without payload"
    "https://example.com/hooks/signup-started?signup_code=newsletter"
    (uri None);
  check
    "keeps the query of the configured url"
    "https://example.com/hooks?signup_code=newsletter&token=secret"
    (uri ~url:(WebhookUrl.of_string "https://example.com/hooks?token=secret") None)
;;

let webhook_job_serialization () =
  let open Signup_code.Webhook in
  let job =
    { url = Data.signup_completed_url
    ; trigger = Trigger.SignUpCompleted
    ; code = Data.code
    ; payload = Some Data.payload
    }
  in
  Alcotest.(check (result testable_webhook Test_utils.error))
    "decode restores the encoded webhook"
    (Ok job)
    (job |> Job.encode |> Job.decode)
;;

let resend_webhook_job () =
  let open Signup_code in
  let webhook =
    { Webhook.url = Data.signup_started_url
    ; trigger = Webhook.Trigger.SignUpStarted
    ; code = Data.code
    ; payload = Some Data.payload
    }
  in
  let failed_job =
    Pool_queue.(
      Instance.create
        ~status:Status.Failed
        Test_utils.Data.database_label
        JobName.SendWebhook
        (Webhook.Job.encode webhook))
  in
  let events = Cqrs_command.Queue_command.Resend.handle failed_job in
  let expected =
    Ok
      [ WebhookResent (webhook, Pool_queue.Instance.id failed_job)
        |> Pool_event.signupcode
      ]
  in
  Test_utils.check_result ~msg:"Resend failed webhook job" expected events
;;

let guest_query_keeps_code_and_payload () =
  let query_parameters =
    Pool_context.Utils.url_parameters_by_user
      (Test_request.mock_request
         ~target:"/signup?signup-code=newsletter&payload=click%3D42&unknown=1"
         ())
      Pool_context.Guest
  in
  Alcotest.(check (list (pair string string)))
    "code and decoded payload are kept"
    [ "signup_code", "newsletter"; "payload", "click=42" ]
    (query_parameters |> CCList.map (CCPair.map_fst Field.show))
;;

let edit_page_shows_signup_url_and_webhooks () =
  let signup_code = Data.with_webhooks () in
  let html =
    Page.Admin.SignupCodes.edit
      (Test_request.mock_context ())
      (Pool_tenant.Url.of_string "pool.example.com")
      signup_code
    |> Format.asprintf "%a" (Tyxml.Html.pp_elt ())
  in
  let contains msg sub = Alcotest.(check bool) msg true (CCString.mem ~sub html) in
  contains "signup url" "pool.example.com/signup?signup-code=newsletter";
  contains "started webhook url" Data.signup_started_url_string;
  contains "completed webhook url" Data.signup_completed_url_string;
  contains "started webhook input" "name=\"signup_started_webhook_url\"";
  contains "completed webhook input" "name=\"signup_completed_webhook_url\""
;;

let persist _ () =
  let open Signup_code in
  Test_utils.Data.connection
  @@ fun db_ctx ->
  let code = Data.unique_code "persisted" in
  let created = create ~signup_started_webhook_url:Data.signup_started_url code in
  let%lwt () = Created created |> handle_event db_ctx in
  let%lwt found = find_by_code db_ctx code in
  Alcotest.(check (option testable_signup_code)) "find created code" (Some created) found;
  let updated =
    { created with
      signup_started_webhook_url = None
    ; signup_completed_webhook_url = Some Data.signup_completed_url
    }
  in
  let%lwt () = Updated updated |> handle_event db_ctx in
  let%lwt found = find db_ctx created.id in
  Alcotest.(check (result testable_signup_code Test_utils.error))
    "find updated code"
    (Ok updated)
    found;
  Lwt.return_unit
;;

let counting_keeps_webhooks _ () =
  let open Signup_code in
  Test_utils.Data.connection
  @@ fun db_ctx ->
  let code = Data.unique_code "counted" in
  let created = create ~signup_completed_webhook_url:Data.signup_completed_url code in
  let%lwt () = Created created |> handle_event db_ctx in
  let%lwt () = SignedUp code |> handle_event db_ctx in
  let%lwt () = Verified code |> handle_event db_ctx in
  let%lwt found = find_by_code db_ctx code in
  Alcotest.(check (option (pair int int)))
    "signup and verification were counted"
    (Some (1, 1))
    (found
     |> CCOption.map (fun { signup_count; verification_count; _ } ->
       Count.value signup_count, Count.value verification_count));
  Alcotest.(check (option testable_webhook))
    "webhook is still configured"
    (Signup_code.webhook Webhook.Trigger.SignUpCompleted created)
    (CCOption.bind found (Signup_code.webhook Webhook.Trigger.SignUpCompleted));
  Lwt.return_unit
;;

let unregistered_code_has_no_webhook _ () =
  let open Signup_code in
  Test_utils.Data.connection
  @@ fun db_ctx ->
  let code = Data.unique_code "unregistered" in
  let%lwt () = SignedUp code |> handle_event db_ctx in
  let%lwt found = find_webhook db_ctx Webhook.Trigger.SignUpStarted code in
  Alcotest.(check (option testable_webhook))
    "no webhook for a code without urls"
    None
    found;
  Lwt.return_unit
;;

let triggered_webhook_is_queued _ () =
  let open Signup_code in
  Test_utils.Data.connection
  @@ fun db_ctx ->
  let count () =
    Pool_queue.(count_workable JobName.SendWebhook) db_ctx |> Lwt.map get_exn
  in
  let code = Data.unique_code "queued" in
  let created = create ~signup_started_webhook_url:Data.signup_started_url code in
  let%lwt () = Created created |> handle_event db_ctx in
  let%lwt before = count () in
  let%lwt webhook =
    find_webhook db_ctx Webhook.Trigger.SignUpStarted ~payload:Data.payload code
    |> Lwt.map (CCOption.get_exn_or "webhook not found")
  in
  let%lwt () = WebhookTriggered webhook |> handle_event db_ctx in
  let%lwt after = count () in
  Alcotest.(check int) "one webhook job was queued" (before + 1) after;
  Lwt.return_unit
;;

module WebhookEndpoint = struct
  type request =
    { meth : Cohttp.Code.meth
    ; uri : Uri.t
    }

  let free_port () =
    let socket = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
    Unix.bind socket (Unix.ADDR_INET (Unix.inet_addr_loopback, 0));
    let port =
      match Unix.getsockname socket with
      | Unix.ADDR_INET (_, port) -> port
      | Unix.ADDR_UNIX _ -> failwith "Expected an inet socket"
    in
    Unix.close socket;
    port
  ;;

  let with_endpoint ~status run =
    let open Cohttp_lwt_unix in
    let port = free_port () in
    let received = ref None in
    let stop, stop_server = Lwt.wait () in
    let callback _ request _ =
      received := Some { meth = Request.meth request; uri = Request.uri request };
      Server.respond_string ~status ~body:"" ()
    in
    let server =
      Server.create ~stop ~mode:(`TCP (`Port port)) (Server.make ~callback ())
    in
    let%lwt result =
      Format.asprintf "http://127.0.0.1:%i/hook" port
      |> Signup_code.WebhookUrl.of_string
      |> run
    in
    Lwt.wakeup_later stop_server ();
    let%lwt () = server in
    Lwt.return (result, !received)
  ;;
end

let send_webhook ~status =
  let open Signup_code in
  Test_utils.Data.connection
  @@ fun db_ctx ->
  let webhook url =
    { Webhook.url
    ; trigger = Webhook.Trigger.SignUpCompleted
    ; code = Data.code
    ; payload = Some Data.payload
    }
  in
  WebhookEndpoint.with_endpoint ~status (fun url ->
    let webhook = webhook url in
    Pool_queue.Job.handle Webhook.Job.send (Database.Any db_ctx) webhook
    |> Lwt.map (fun result -> result, webhook))
;;

let webhook_job_gets_url_with_query _ () =
  let%lwt (result, webhook), received = send_webhook ~status:`OK in
  Alcotest.(check (result unit Test_utils.error)) "job succeeds" (Ok ()) result;
  let meth, uri =
    match received with
    | None -> Alcotest.fail "endpoint was not called"
    | Some { WebhookEndpoint.meth; uri } -> meth, uri
  in
  Alcotest.(check string) "uses GET" "GET" (Cohttp.Code.string_of_method meth);
  Alcotest.(check string)
    "sends code and payload as query parameters"
    (Signup_code.Webhook.request_uri webhook |> Uri.path_and_query)
    (Uri.path_and_query uri);
  Lwt.return_unit
;;

let webhook_job_fails_on_error_status _ () =
  let%lwt (result, _), _ = send_webhook ~status:`Internal_server_error in
  Alcotest.(check bool)
    "job fails so that the queue retries"
    true
    (CCResult.is_error result);
  Lwt.return_unit
;;

let webhook_job_fails_when_unreachable _ () =
  let open Signup_code in
  let url =
    Format.asprintf "http://127.0.0.1:%i/hook" (WebhookEndpoint.free_port ())
    |> WebhookUrl.of_string
  in
  let%lwt result =
    Test_utils.Data.connection
    @@ fun db_ctx ->
    Pool_queue.Job.handle
      Webhook.Job.send
      (Database.Any db_ctx)
      { Webhook.url
      ; trigger = Webhook.Trigger.SignUpStarted
      ; code = Data.code
      ; payload = None
      }
  in
  Alcotest.(check bool)
    "job fails so that the queue retries"
    true
    (CCResult.is_error result);
  Lwt.return_unit
;;

let signup_request_resolves_webhook _ () =
  let open Signup_code in
  Test_utils.Data.connection
  @@ fun db_ctx ->
  let code = Data.unique_code "requested" in
  let created = create ~signup_started_webhook_url:Data.signup_started_url code in
  let%lwt () = Created created |> handle_event db_ctx in
  let query_parameters =
    [ url_key, Code.value code; payload_url_key, Payload.value Data.payload ]
  in
  let resolve = Handler.Contact.SignUp.signup_code_of_query db_ctx query_parameters in
  let%lwt found_code, found_payload, started = resolve Webhook.Trigger.SignUpStarted in
  let%lwt _, _, completed = resolve Webhook.Trigger.SignUpCompleted in
  Alcotest.(check (option string))
    "code of the request"
    (Some (Code.value code))
    (CCOption.map Code.value found_code);
  Alcotest.(check (option string))
    "payload of the request"
    (Some (Payload.value Data.payload))
    (CCOption.map Payload.value found_payload);
  Alcotest.(check (option testable_webhook))
    "started webhook carries the payload"
    (Some
       { Webhook.url = Data.signup_started_url
       ; trigger = Webhook.Trigger.SignUpStarted
       ; code
       ; payload = Some Data.payload
       })
    started;
  Alcotest.(check (option testable_webhook))
    "no completed webhook configured"
    None
    completed;
  Lwt.return_unit
;;

let verification_mail_keeps_code_and_payload _ () =
  let open Utils.Lwt_result.Infix in
  Test_utils.Data.connection
  @@ fun db_ctx ->
  let%lwt tenant = Pool_tenant.find_by_db_ctx db_ctx ||> get_exn in
  let%lwt token = Pool_token.create db_ctx [ "email", "jane@example.com" ] in
  let%lwt dispatch =
    Message_template.SignUpVerification.create
      ~signup_code:Data.code
      ~signup_payload:(Signup_code.Payload.of_string "click42")
      db_ctx
      Pool_common.Language.En
      tenant
      (Pool_user.EmailAddress.of_string "jane@example.com")
      token
      (Pool_user.Firstname.of_string "Jane")
      (Pool_user.Lastname.of_string "Doe")
      (Pool_user.Id.create ())
  in
  let { Sihl_email.text; _ } = dispatch |> Email.job |> Email.Service.Job.email in
  let contains msg sub = Alcotest.(check bool) msg true (CCString.mem ~sub text) in
  contains "verification link keeps the code" "signup-code=newsletter";
  contains "verification link keeps the payload" "payload=click42";
  Lwt.return_unit
;;

let create_form_persists_code _ () =
  let open Utils.Lwt_result.Infix in
  let open Signup_code in
  Test_utils.Data.connection
  @@ fun db_ctx ->
  let%lwt tenant = Pool_tenant.find_by_db_ctx db_ctx ||> get_exn in
  let code = Data.unique_code "form" in
  let submit signup_started_url =
    Test_request.mock_post_request
      ~context_tenant:tenant
      ~target:"/admin/settings/signup-code"
      [ Field.(show Code), [ Code.value code ]
      ; Field.(show SignUpStartedWebhookUrl), [ signup_started_url ]
      ]
    |> Handler.Admin.SignupCodes.create
    ||> fun { Rock.Response.status; _ } -> Httpaf.Status.to_code status
  in
  let find_signup_started_url () =
    find_by_code db_ctx code
    ||> CCOption.map (fun { signup_started_webhook_url; _ } ->
      CCOption.map WebhookUrl.value signup_started_webhook_url)
  in
  let%lwt status = submit Data.signup_started_url_string in
  Alcotest.(check int) "redirects after creating the code" 302 status;
  let%lwt created = find_signup_started_url () in
  Alcotest.(check (option (option string)))
    "code is stored with its webhook"
    (Some (Some Data.signup_started_url_string))
    created;
  let%lwt status = submit Data.signup_completed_url_string in
  Alcotest.(check int) "rejects an existing code" 400 status;
  let%lwt unchanged = find_signup_started_url () in
  Alcotest.(check (option (option string))) "existing code is unchanged" created unchanged;
  Lwt.return_unit
;;
