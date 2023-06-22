module Queue = Sihl_queue.MariaDb
open Entity
open CCFun
module PhoneNumber = Pool_user.PhoneNumber

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

module Config = struct
  let gateway_server = "https://rest.gtx-messaging.net"
  let gateway_api_path = "smsc/sendsms"

  let gateway_url auth_key =
    Format.asprintf "%s/%s/%s" gateway_server gateway_api_path auth_key
  ;;

  let auth_key () = Sihl.Configuration.read_string "GTX_AUTH_KEY"
end

let get_api_key database_label =
  let open Utils.Lwt_result.Infix in
  Pool_tenant.find_gtx_api_key_by_label database_label
  ||> Pool_common.Utils.get_or_failwith
;;

let request_body { recipient; text; sender } =
  [ "from", [ Pool_tenant.Title.value sender ]
  ; "to", [ PhoneNumber.value recipient ]
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
    (PhoneNumber.value recipient)
    text
;;

let print_message ~tags ?(log_level = Logs.Info) msg =
  let text_message = format_message msg in
  Logs.msg ~src log_level (fun m -> m ~tags "%s" text_message)
;;

let intercept_message ~tags database_label msg =
  let send_intercept_message recipient =
    Logs.info ~src (fun m ->
      m
        ~tags
        "Sending text message intercepted. Sending message as email to ('%s')"
        (Pool_user.EmailAddress.value recipient));
    let%lwt sender =
      Email.Service.sender_of_pool database_label
      |> Lwt.map Settings.ContactEmail.value
    in
    let body = format_message msg in
    let subject = "Text message intercept" in
    Sihl_email.create
      ~sender
      ~recipient:(Pool_user.EmailAddress.value recipient)
      ~subject
      ~html:body
      body
    |> Email.Service.dispatch database_label
  in
  match Sihl.Configuration.is_production () || bypass () with
  | false -> print_message ~tags msg |> Lwt.return_some
  | true ->
    let intercept_address =
      match
        Sihl.Configuration.(
          is_production (), read_string "TEXT_MESSAGE_INTERCEPT_ADDRESS")
      with
      | true, _ | _, None -> None
      | false, Some address -> Some address
    in
    intercept_address
    |> CCOption.map_or
         ~default:(() |> Lwt.return_some)
         CCFun.(
           Pool_user.EmailAddress.of_string
           %> send_intercept_message
           %> Lwt.map CCOption.return)
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
  let database_label =
    let open CCOption in
    ctx
    >>= CCList.assoc_opt ~eq:( = ) "pool"
    >|= Pool_database.Label.create %> Pool_common.Utils.get_or_failwith
    |> get_exn_or "Invalid context passed!"
  in
  let%lwt api_key = get_api_key database_label in
  let tags = tags database_label in
  match%lwt intercept_message ~tags database_label msg with
  | Some () -> Lwt_result.return ()
  | None ->
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
           (PhoneNumber.value msg.recipient)
           msg.text
           body_string);
       Lwt.return_ok ())
;;

let test_api_key ~tags database_label api_key phone_number tenant_title =
  let open Cohttp in
  let msg = create phone_number tenant_title "Your API Key is valid." in
  match%lwt intercept_message ~tags database_label msg with
  | Some () -> Lwt_result.return api_key
  | None ->
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
           (PhoneNumber.value msg.recipient)
           msg.text
           body_string);
       Lwt.return_ok api_key)
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
  Logs.debug ~src (fun m ->
    m
      ~tags:(Pool_database.Logger.Tags.create database_label)
      "Dispatch text message to %s"
      (Pool_user.PhoneNumber.value msg.recipient));
  Queue.dispatch ~ctx:(Pool_database.to_ctx database_label) msg Job.send
;;
