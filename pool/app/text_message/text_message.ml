include Entity
module PhoneNumber = Pool_user.PhoneNumber

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

let src = Logs.Src.create "pool_tenant.service.text_message"
let tags database_label = Pool_database.(Logger.Tags.create database_label)

let request_body recipient text sender =
  [ "from", [ Pool_tenant.Title.value sender ]
  ; "to", [ PhoneNumber.value recipient ]
  ; "text", [ text ]
  ]
;;

let response_to_string res =
  Cohttp_lwt.Response.pp_hum Format.str_formatter res;
  Format.flush_str_formatter ()
;;

let intercept_message ~tags ?(log_level = Logs.Info) { recipient; text; sender }
  =
  Logs.msg ~src log_level (fun m ->
    m
      ~tags
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
      text);
  Lwt.return_unit
;;

let intercept_phone_number ~tags recipient =
  Sihl.Configuration.read_string "TEST_PHONE_NUMBER"
  |> CCOption.map PhoneNumber.of_string
  |> function
  | None -> recipient
  | Some intercept_recipient ->
    Logs.info ~src (fun m ->
      m
        ~tags
        "Sending text message intercepted. Sending message to new recipient \
         ('%s')"
        (PhoneNumber.value intercept_recipient));
    intercept_recipient
;;

let send database_label ({ recipient; text; sender } as m) =
  let tags = tags database_label in
  match Sihl.Configuration.is_production () || bypass () with
  | false -> intercept_message ~tags m
  | true ->
    let open Cohttp in
    let open Cohttp_lwt_unix in
    (match Config.auth_key () with
     | None -> failwith "Undefined 'GTX_AUTH_KEY'"
     | Some auth_key ->
       let recipient = intercept_phone_number ~tags recipient in
       let body =
         request_body recipient text sender |> Cohttp_lwt.Body.of_form
       in
       let%lwt resp, body =
         Client.post ~body (Uri.of_string (Config.gateway_url auth_key))
       in
       let%lwt body_string = Cohttp_lwt.Body.to_string body in
       let%lwt () = Cohttp_lwt.Body.drain_body body in
       (match
          resp |> Response.status |> Code.code_of_status |> CCInt.equal 200
        with
        | false ->
          Logs.err ~src (fun m ->
            m
              ~tags
              "Could not send text message: %s\nresponse: %s"
              body_string
              (response_to_string resp));
          Lwt.return_unit
        | true ->
          Logs.info ~src (fun m ->
            m
              ~tags
              "Send text message to %s: %s\n%s"
              (PhoneNumber.value recipient)
              text
              body_string);
          Lwt.return_unit))
;;

type event =
  | Sent of Entity.t
  | BulkSent of t list
[@@deriving eq, show]

let handle_event pool : event -> unit Lwt.t = function
  | Sent message -> send pool message
  | BulkSent messages -> Lwt_list.iter_s (fun msg -> send pool msg) messages
;;
