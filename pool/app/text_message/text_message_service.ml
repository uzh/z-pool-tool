(* TODO: Use queue *)

open Entity
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
  Logs.msg ~src log_level (fun m -> m ~tags "%s" text_message);
  Lwt.return_unit
;;

let intercept_message ~tags database_label message recipient =
  Logs.info ~src (fun m ->
    m
      ~tags
      "Sending text message intercepted. Sending message as email to ('%s')"
      (Pool_user.EmailAddress.value recipient));
  let sender =
    Sihl.Configuration.read_string "SMTP_SENDER"
    |> CCOption.get_exn_or "Undefined 'SMTP_SENDER'"
  in
  let body = format_message message in
  let subject = "Text message intercept" in
  (* TODO: formatting, where to place this module??? *)
  Sihl_email.
    { sender
    ; recipient = Pool_user.EmailAddress.value recipient
    ; subject
    ; text = body
    ; html = Some body
    ; cc = []
    ; bcc = []
    }
  |> Email.Service.dispatch database_label
;;

let send database_label api_key ({ recipient; text; sender } as m) =
  let tags = tags database_label in
  match Sihl.Configuration.is_production () || bypass () with
  | false -> print_message ~tags m
  | true ->
    (match Sihl.Configuration.read_string "TEXT_MESSAGE_INTERCEPT_ADDRESS" with
     | Some email ->
       email
       |> Pool_user.EmailAddress.of_string
       |> intercept_message database_label ~tags m
     | None ->
       let open Cohttp in
       let open Cohttp_lwt_unix in
       let body =
         request_body recipient text sender |> Cohttp_lwt.Body.of_form
       in
       let%lwt resp, body =
         let url =
           api_key |> Pool_tenant.GtxApiKey.value |> Config.gateway_url
         in
         Logs.info (fun m -> m "%s" url);
         url |> Uri.of_string |> Client.post ~body
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
