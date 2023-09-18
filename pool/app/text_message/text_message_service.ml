open CCFun
open Utils.Lwt_result.Infix
open Entity
module Queue = Sihl_queue.MariaDb
module CellPhone = Pool_user.CellPhone

type message =
  | TextMessageJob of t
  | EmailJob of Email.job

let src = Logs.Src.create "pool_tenant.service.text_message"
let tags database_label = Pool_database.(Logger.Tags.create database_label)
let start () = Lwt.return_unit
let stop () = Lwt.return_unit

let lifecycle =
  Sihl.Container.create_lifecycle
    "text_messages"
    ~dependencies:(fun () -> [ Sihl.Database.lifecycle ])
    ~start
    ~stop
;;

let register () =
  let configuration = Sihl.Configuration.make () in
  Sihl.Container.Service.create ~configuration lifecycle
;;

let bypass () =
  CCOption.get_or
    ~default:false
    (Sihl.Configuration.read_bool "TEXT_MESSAGE_BYPASS_INTERCEPT")
;;

let incercept_text_message_address () =
  Sihl.Configuration.read_string "TEXT_MESSAGE_INTERCEPT_ADDRESS"
;;

module Config = struct
  let gateway_server = "https://rest.gtx-messaging.net"
  let gateway_api_path = "smsc/sendsms"

  let gateway_url auth_key =
    Format.asprintf "%s/%s/%s" gateway_server gateway_api_path auth_key
  ;;
end

let get_api_key database_label =
  let open Utils.Lwt_result.Infix in
  Pool_tenant.find_gtx_api_key_by_label database_label
  ||> Pool_common.Utils.get_or_failwith
;;

let request_body { recipient; text; sender } =
  [ "from", [ Pool_tenant.Title.value sender ]
  ; "to", [ CellPhone.value recipient ]
  ; "text", [ text ]
  ]
;;

let response_to_string res =
  Cohttp_lwt.Response.pp_hum Format.str_formatter res;
  Format.flush_str_formatter ()
;;

let format_message { recipient; text; sender } =
  Format.asprintf
    {|
-----------------------
Message sent by: %s
Recipient: %s
-----------------------
Text:
%s
-----------------------
    |}
    (Pool_tenant.Title.value sender)
    (CellPhone.value recipient)
    text
;;

let print_message ~tags ?(log_level = Logs.Info) msg =
  let text_message = format_message msg in
  Logs.msg ~src log_level (fun m -> m ~tags "%s" text_message)
;;

let intercept_prepare database_label ({ recipient; _ } as job) =
  let tags = tags database_label in
  let () =
    if Sihl.Configuration.is_development ()
    then print_message ~tags ~log_level:Logs.Info job
    else ()
  in
  match
    ( Sihl.Configuration.is_production () || bypass ()
    , incercept_text_message_address () )
  with
  | true, _ -> Lwt.return_ok (TextMessageJob job)
  | false, Some new_recipient ->
    Logs.info ~src (fun m ->
      m
        ~tags
        "Sending text message intercepted. Sending message as email to ('%s')"
        new_recipient);
    let%lwt sender =
      Email.Service.default_sender_of_pool database_label
      |> Lwt.map Pool_user.EmailAddress.value
    in
    let subject =
      Format.asprintf
        "[Pool Tool] Text message intercept (original to: %s)"
        (CellPhone.value recipient)
    in
    EmailJob
      { Email.email =
          Sihl_email.create
            ~sender
            ~recipient:new_recipient
            ~subject
            (format_message job)
      ; smtp_auth_id = None
      }
    |> Lwt.return_ok
  | false, None ->
    Lwt.return_error
      (Pool_common.Message.TextMessageInterceptionError
         "Sending text message intercepted! As no redirect email is specified \
          it/they wont be sent. Please define environment variable \
          'TEXT_MESSAGE_INTERCEPT_ADDRESS'.")
;;

let send_message api_key msg =
  let open Cohttp_lwt_unix in
  let body = request_body msg |> Cohttp_lwt.Body.of_form in
  let%lwt resp, body =
    api_key
    |> Pool_tenant.GtxApiKey.value
    |> Config.gateway_url
    |> Uri.of_string
    |> Client.post ~body
  in
  let%lwt body_string = Cohttp_lwt.Body.to_string body in
  let%lwt () = Cohttp_lwt.Body.drain_body body in
  Lwt.return (resp, body_string)
;;

let handle ?ctx msg =
  let open Sihl.Configuration in
  let database_label =
    let open CCOption in
    ctx
    >>= CCList.assoc_opt ~eq:( = ) "pool"
    >|= Pool_database.Label.create %> Pool_common.Utils.get_or_failwith
    |> get_exn_or "Invalid context passed!"
  in
  let%lwt api_key = get_api_key database_label in
  let tags = tags database_label in
  match is_production () || bypass () with
  | true ->
    let open Cohttp in
    let%lwt resp, body_string = send_message api_key msg in
    (match
       resp |> Response.status |> Code.code_of_status |> CCInt.equal 200
     with
     | false ->
       let error =
         Format.asprintf
           "Could not send text message: %s\nresponse: %s"
           body_string
           (response_to_string resp)
       in
       Logs.err ~src (fun m -> m ~tags "%s" error);
       Lwt.return_error error
     | true ->
       Logs.info ~src (fun m ->
         m
           ~tags
           "Send text message to %s: %s\n%s"
           (CellPhone.value msg.recipient)
           msg.text
           body_string);
       Lwt.return_ok ())
  | false ->
    "Sending text message intercepted (non production environment)."
    |> Lwt.return_error
;;

let test_api_key ~tags api_key cell_phone tenant_title =
  let open Sihl.Configuration in
  let open Cohttp in
  let msg = create cell_phone tenant_title "Your API Key is valid." in
  let () = if is_development () then print_message ~tags msg else () in
  match is_production () || bypass () with
  | true ->
    let%lwt resp, body_string = send_message api_key msg in
    (match
       resp |> Response.status |> Code.code_of_status |> CCInt.equal 200
     with
     | false ->
       let error =
         Format.asprintf
           "Verifying API Key: Could not send text message: %s\nresponse: %s"
           body_string
           (response_to_string resp)
       in
       Logs.err ~src (fun m -> m ~tags "%s" error);
       Lwt.return_error Pool_common.Message.(Invalid Field.GtxApiKey)
     | true ->
       Logs.info ~src (fun m ->
         m
           ~tags
           "Verifying API Key: Send text message to %s: %s\n%s"
           (CellPhone.value msg.recipient)
           msg.text
           body_string);
       Lwt.return_ok api_key)
  | false ->
    Pool_common.Message.TextMessageInterceptionError
      "Sending text message intercepted"
    |> Lwt.return_error
;;

module Job = struct
  let send =
    let encode = Entity.yojson_of_t %> Yojson.Safe.to_string in
    let decode msg =
      try Ok (t_of_yojson (Yojson.Safe.from_string msg)) with
      | Yojson.Json_error msg ->
        Logs.err ~src (fun m ->
          m
            ~tags:Pool_database.(Logger.Tags.create root)
            "Serialized message string was NULL, can not deserialize message. \
             Please fix the string manually and reset the job instance. Error: \
             %s"
            msg);
        Error "Invalid serialized message string received"
    in
    Sihl.Contract.Queue.create_job
      handle
      ~max_tries:10
      ~retry_delay:(Sihl.Time.Span.hours 1)
      encode
      decode
      "send_text_message"
  ;;
end

let send database_label msg =
  let ctx = Pool_database.to_ctx database_label in
  Logs.debug ~src (fun m ->
    m
      ~tags:(Pool_database.Logger.Tags.create database_label)
      "Dispatch text message to %s"
      (Pool_user.CellPhone.value msg.recipient));
  intercept_prepare database_label msg
  ||> Pool_common.Utils.get_or_failwith
  >|> function
  | TextMessageJob job -> Queue.dispatch ~ctx job Job.send
  | EmailJob job -> Queue.dispatch ~ctx job Email.Service.Job.send
;;
