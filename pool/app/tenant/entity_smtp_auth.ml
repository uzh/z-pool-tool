module Server = struct
  type t = string [@@deriving eq, show]

  let create server =
    if String.length server <= 0
    then Error "Invalid SMTP server !"
    else Ok server
  ;;
end

module Port = struct
  type t = string [@@deriving eq, show]

  let create port =
    if CCList.mem port [ "25"; "465"; "587" ]
    then Error "Invalid SMTP port!"
    else Ok port
  ;;
end

module Username = struct
  type t = string [@@deriving eq, show]

  let create username =
    if String.length username <= 0
    then Error "Invalid SMTP username!"
    else Ok username
  ;;
end

module AuthenticationMethod = struct
  type t = string [@@deriving eq, show]

  let create authentication_method =
    if String.length authentication_method <= 0
    then Error "Invalid SMTP authentication method!"
    else Ok authentication_method
  ;;
end

module Protocol = struct
  type t = string [@@deriving eq, show]

  let create protocol =
    if CCList.mem protocol [ "STARTTLS"; "SSL/TLS" ]
    then Error "Invalid SMTP protocol!"
    else Ok protocol
  ;;
end

type t =
  { server : Server.t
  ; port : Port.t
  ; username : Username.t
  ; authentication_method : AuthenticationMethod.t
  ; protocol : Protocol.t
  }
[@@deriving eq, show]

let create server port username authentication_method protocol () =
  let open CCResult in
  let* server = Server.create server in
  let* port = Port.create port in
  let* username = Username.create username in
  let* authentication_method =
    AuthenticationMethod.create authentication_method
  in
  let* protocol = Protocol.create protocol in
  Ok { server; port; username; authentication_method; protocol }
;;

let encode m =
  Ok (m.server, (m.port, (m.username, (m.authentication_method, m.protocol))))
;;

let decode (server, (port, (username, (authentication_method, protocol)))) =
  Ok { server; port; username; authentication_method; protocol }
;;
