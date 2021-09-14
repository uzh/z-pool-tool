module Server : sig
  type t

  val equal : t -> t -> bool
  val pp : Format.formatter -> t -> unit
  val show : t -> string
  val create : string -> (t, string) result
end = struct
  type t = string [@@deriving eq, show]

  let create server =
    if String.length server <= 0
    then Error "Invalid SMTP server !"
    else Ok server
  ;;
end

module Port : sig
  type t

  val equal : t -> t -> bool
  val pp : Format.formatter -> t -> unit
  val show : t -> string
  val create : string -> (t, string) result
end = struct
  type t = string [@@deriving eq, show]

  let create port =
    if CCList.mem port [ "25"; "465"; "587" ]
    then Error "Invalid SMTP port!"
    else Ok port
  ;;
end

module Username : sig
  type t

  val equal : t -> t -> bool
  val pp : Format.formatter -> t -> unit
  val show : t -> string
  val create : string -> (t, string) result
end = struct
  type t = string [@@deriving eq, show]

  let create username =
    if String.length username <= 0
    then Error "Invalid SMTP username!"
    else Ok username
  ;;
end

module AuthenticationMethod : sig
  type t

  val equal : t -> t -> bool
  val pp : Format.formatter -> t -> unit
  val show : t -> string
  val create : string -> (t, string) result
end = struct
  type t = string [@@deriving eq, show]

  let create authentication_method =
    if String.length authentication_method <= 0
    then Error "Invalid SMTP authentication method!"
    else Ok authentication_method
  ;;
end

module Protocol : sig
  type t

  val equal : t -> t -> bool
  val pp : Format.formatter -> t -> unit
  val show : t -> string
  val create : string -> (t, string) result
end = struct
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
  Ok
    ( Server.show m.server
    , ( Port.show m.port
      , ( Username.show m.username
        , ( AuthenticationMethod.show m.authentication_method
          , Protocol.show m.protocol ) ) ) )
;;

let decode (server, (port, (username, (authentication_method, protocol)))) =
  let ( let* ) = Result.bind in
  let* server = Server.create server in
  let* port = Port.create port in
  let* username = Username.create username in
  let* authentication_method =
    AuthenticationMethod.create authentication_method
  in
  let* protocol = Protocol.create protocol in
  Ok { server; port; username; authentication_method; protocol }
;;
