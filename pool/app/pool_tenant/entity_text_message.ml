(** TODO

    - handle gtx errors (eg. not delivered)

    - add interceptor *)

module Config = struct
  let gateway_server = "https://rest.gtx-messaging.net"
  let gateway_api_path = "smsc/sendsms"

  let gateway_url auth_key =
    Format.asprintf "%s/%s/%s" gateway_server gateway_api_path auth_key
  ;;

  let gateway_port = 443
  let auth_key () = Sihl.Configuration.read_string "GTX_AUTH_KEY"
  let default_sender = "UAST"
end

let src = Logs.Src.create "pool_tenant.service.text_message"
let tags database_label = Pool_database.(Logger.Tags.create database_label)

let request_body recipient text =
  [ "from", [ Config.default_sender ]; "to", [ recipient ]; "text", [ text ] ]
;;

let response_to_string res =
  Cohttp_lwt.Response.pp_hum Format.str_formatter res;
  Format.flush_str_formatter ()
;;

let send database_label ~text ~recipient =
  if Sihl.Configuration.is_production () |> not
  then Lwt.return_unit
  else
    let open Cohttp in
    let open Cohttp_lwt_unix in
    match Config.auth_key () with
    | None -> failwith "Undefined 'GTX_AUTH_KEY'"
    | Some auth_key ->
      let recipient = Pool_user.PhoneNumber.value recipient in
      let body = request_body recipient text |> Cohttp_lwt.Body.of_form in
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
             ~tags:(tags database_label)
             "Could not send text message: %s\nresponse: %s"
             body_string
             (response_to_string resp));
         Lwt.return_unit
       | true ->
         Logs.info ~src (fun m ->
           m
             ~tags:(Pool_database.Logger.Tags.create database_label)
             "Send text message to %s: %s\n%s"
             recipient
             text
             body_string);
         Lwt.return_unit)
;;
