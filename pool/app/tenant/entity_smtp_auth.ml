module Server = struct
  type t = string [@@deriving eq, show]

  let value t = t

  let create server =
    if String.length server <= 0
    then Error "Invalid SMTP server !"
    else Ok server
  ;;

  let schema () =
    Conformist.custom
      (fun l -> l |> List.hd |> create)
      (fun l -> [ value l ])
      "smtp_auth_server"
  ;;
end

module Port = struct
  type t = string [@@deriving eq, show]

  let value t = t

  let create port =
    if CCList.mem port [ "25"; "465"; "587" ]
    then Ok port
    else Error "Invalid SMTP port!"
  ;;

  let schema () =
    Conformist.custom
      (fun l -> l |> List.hd |> create)
      (fun l -> [ value l ])
      "smtp_auth_port"
  ;;
end

module Username = struct
  type t = string [@@deriving eq, show]

  let value t = t

  let create username =
    if String.length username <= 0
    then Error "Invalid SMTP username!"
    else Ok username
  ;;

  let schema () =
    Conformist.custom
      (fun l -> l |> List.hd |> create)
      (fun l -> [ value l ])
      "smtp_auth_username"
  ;;
end

module AuthenticationMethod = struct
  type t = string [@@deriving eq, show]

  let value t = t

  let create authentication_method =
    if String.length authentication_method <= 0
    then Error "Invalid SMTP authentication method!"
    else Ok authentication_method
  ;;

  let schema () =
    Conformist.custom
      (fun l -> l |> List.hd |> create)
      (fun l -> [ value l ])
      "smtp_auth_authentication_method"
  ;;
end

module Protocol = struct
  type t = string [@@deriving eq, show]

  let value t = t

  let create protocol =
    if CCList.mem protocol [ "STARTTLS"; "SSL/TLS" ]
    then Ok protocol
    else Error "Invalid SMTP protocol!"
  ;;

  let schema () =
    Conformist.custom
      (fun l -> l |> List.hd |> create)
      (fun l -> [ value l ])
      "smtp_auth_protocol"
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

let create server port username authentication_method protocol =
  Ok { server; port; username; authentication_method; protocol }
;;
