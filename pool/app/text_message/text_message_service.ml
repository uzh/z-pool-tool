open CCFun
open Utils.Lwt_result.Infix
open Entity
module CellPhone = Pool_user.CellPhone

type message =
  | TextMessageJob of t
  | EmailJob of Email.Service.Job.t

let src = Logs.Src.create "pool_tenant.service.text_message"
let tags database_label = Database.(Logger.Tags.create database_label)
let start () = Lwt.return_unit
let stop () = Lwt.return_unit

let lifecycle =
  Sihl.Container.create_lifecycle
    "text_messages"
    ~dependencies:(fun () -> [ Pool_database.lifecycle ])
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
  Cohttp.Response.pp_hum Format.str_formatter res;
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

let intercept_prepare database_label message =
  let tags = tags database_label in
  let () =
    if Sihl.Configuration.is_development ()
    then print_message ~tags ~log_level:Logs.Info message
    else ()
  in
  match
    ( Sihl.Configuration.is_production () || bypass ()
    , incercept_text_message_address () )
  with
  | true, _ -> Lwt.return_ok (TextMessageJob message)
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
        (CellPhone.value message.recipient)
    in
    EmailJob
      (Sihl_email.create
         ~sender
         ~recipient:new_recipient
         ~subject
         (format_message message)
       |> Email.Service.Job.create)
    |> Lwt.return_ok
  | false, None ->
    Lwt.return_error
      (Pool_message.Error.TextMessageInterceptionError
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
       Lwt.return_error Pool_message.(Error.Invalid Field.GtxApiKey)
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
    let (_ : Pool_message.Error.t) =
      Pool_common.Utils.with_log_error
        ~level:Logs.Warning
        (Pool_message.Error.TextMessageInterceptionError
           "Verifying API Key: Skip validation due to non production \
            environment!")
    in
    api_key |> Lwt.return_ok
;;

module Job = struct
  let encode = yojson_of_t %> Yojson.Safe.to_string

  let decode str =
    try Ok (str |> Yojson.Safe.from_string |> t_of_yojson) with
    | _ ->
      Logs.err ~src (fun m ->
        m
          "Serialized email string was NULL, can not deserialize email. Please \
           fix the string manually and reset the job instance. Error: %s"
          str);
      Error Pool_message.(Error.Invalid Field.Input)
  ;;

  let handle database_label message =
    let open Sihl.Configuration in
    let%lwt api_key = get_api_key database_label in
    let tags = tags database_label in
    match is_production () || bypass () with
    | true ->
      let open Cohttp in
      let%lwt resp, body_string = send_message api_key message in
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
         Logs.debug ~src (fun m -> m ~tags "%s" error);
         Pool_message.Error.TextMessageError error |> Lwt.return_error
       | true ->
         Logs.info ~src (fun m ->
           m
             ~tags
             "Send text message to %s: %s\n%s"
             (CellPhone.value message.recipient)
             message.text
             body_string);
         Lwt.return_ok ())
    | false ->
      Pool_message.Error.TextMessageError
        "Intercepted (non production environment)"
      |> Lwt.return_error
  ;;

  let send =
    let open Pool_queue in
    Job.create
      ~max_tries:10
      ~retry_delay:(Sihl.Time.Span.hours 1)
      handle
      encode
      decode
      JobName.SendTextMessage
  ;;
end

let dispatch
  ?id
  ?new_recipient
  ?message_template
  ?(mappings = Pool_queue.mappings_create [])
  database_label
  message
  =
  let tags = Database.Logger.Tags.create database_label in
  Logs.debug ~src (fun m ->
    let open Pool_user.CellPhone in
    m ~tags "Dispatch text message to %s" (value message.recipient));
  message
  |> update ?new_recipient
  |> intercept_prepare database_label
  ||> Pool_common.Utils.get_or_failwith
  >|> function
  | TextMessageJob job ->
    Pool_queue.dispatch
      ?id
      ?message_template
      ~mappings
      database_label
      job
      Job.send
  | EmailJob job ->
    Email.Service.dispatch ?id ?message_template ~mappings database_label job
;;
