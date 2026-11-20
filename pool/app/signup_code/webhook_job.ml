open CCFun
open Entity.Webhook

let src = Logs.Src.create "signup_code.webhook"
let encode = yojson_of_t %> Yojson.Safe.to_string

let decode str =
  try Ok (str |> Yojson.Safe.from_string |> t_of_yojson) with
  | _ -> Error Pool_message.(Error.Invalid Field.Input)
;;

let show_recipient =
  Pool_queue.Instance.input
  %> decode
  %> CCResult.map_or ~default:"error" (fun { url; _ } -> Entity.WebhookUrl.value url)
;;

let call webhook =
  let open Cohttp_lwt_unix in
  let%lwt response, body = webhook |> request_uri |> Client.get in
  let%lwt () = Cohttp_lwt.Body.drain_body body in
  Lwt.return (response |> Response.status |> Cohttp.Code.code_of_status)
;;

let handle ?id:_ (Database.Any db_ctx) webhook =
  let tags = Database.Logger.Tags.of_db_ctx db_ctx in
  let url = Entity.WebhookUrl.value webhook.url in
  let error msg =
    Pool_message.Error.WebhookError
      [%string "%{Trigger.show webhook.trigger} %{url}: %{msg}"]
  in
  Lwt.catch
    (fun () ->
       match%lwt call webhook with
       | status when Cohttp.Code.is_success status ->
         Logs.info ~src (fun m ->
           m ~tags "Called webhook %s for %s" url (Trigger.show webhook.trigger));
         Lwt.return_ok ()
       | status ->
         Lwt.return_error (error [%string "responded with status %{status#Int}"]))
    (Printexc.to_string %> error %> Lwt.return_error)
;;

let send =
  Pool_queue.Job.create
    ~max_tries:5
    ~retry_delay:(Sihl.Time.Span.minutes 5)
    handle
    encode
    decode
    Pool_queue.JobName.SendWebhook
;;

let dispatch ?job_ctx db_ctx webhook =
  Logs.debug ~src (fun m ->
    m
      ~tags:(Database.Logger.Tags.of_db_ctx db_ctx)
      "Dispatch webhook %s"
      (Entity.WebhookUrl.value webhook.url));
  Pool_queue.dispatch ?job_ctx db_ctx webhook send
;;
