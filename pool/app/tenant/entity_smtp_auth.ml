module PoolError = Pool_common.Message

module Server = struct
  type t = string [@@deriving eq, show]

  let value m = m

  let create server =
    if String.length server <= 0
    then Error PoolError.(Invalid SmtpAuthServer)
    else Ok server
  ;;

  let schema () =
    Conformist.custom
      Pool_common.(Utils.schema_decoder create Message.SmtpAuthServer)
      CCList.pure
      "smtp_auth_server"
  ;;
end

module Port = struct
  type t = string [@@deriving eq, show]

  let value m = m

  let create port =
    if CCList.mem port [ "25"; "465"; "587" ]
    then Ok port
    else Error PoolError.(Invalid SmtpPort)
  ;;

  let schema () =
    Conformist.custom
      Pool_common.(Utils.schema_decoder create Message.SmtpPort)
      CCList.pure
      "smtp_auth_port"
  ;;
end

module Username = struct
  type t = string [@@deriving eq, show]

  let value m = m

  let create username =
    if String.length username <= 0
    then Error PoolError.(Invalid SmtpUsername)
    else Ok username
  ;;

  let schema () =
    Conformist.custom
      Pool_common.(Utils.schema_decoder create Message.SmtpUsername)
      CCList.pure
      "smtp_auth_username"
  ;;
end

module Password = struct
  type t = string [@@deriving eq, show]

  let show m = CCString.repeat "*" @@ CCString.length m

  let create password =
    if String.length password <= 0
    then Error PoolError.(Invalid SmtpPassword)
    else Ok password
  ;;

  let schema () =
    Conformist.custom
      Pool_common.(Utils.schema_decoder create Message.SmtpPassword)
      CCList.pure
      "smtp_auth_password"
  ;;
end

module AuthenticationMethod = struct
  type t = string [@@deriving eq, show]

  let value m = m

  let create authentication_method =
    if String.length authentication_method <= 0
    then Error PoolError.(Invalid SmtpAuthMethod)
    else Ok authentication_method
  ;;

  let schema () =
    Conformist.custom
      Pool_common.(Utils.schema_decoder create Message.SmtpAuthMethod)
      CCList.pure
      "smtp_auth_authentication_method"
  ;;
end

module Protocol = struct
  type t = string [@@deriving eq, show]

  let value m = m

  let create protocol =
    if CCList.mem protocol [ "STARTTLS"; "SSL/TLS" ]
    then Ok protocol
    else Error PoolError.(Invalid SmtpProtocol)
  ;;

  let schema () =
    Conformist.custom
      Pool_common.(Utils.schema_decoder create Message.SmtpProtocol)
      CCList.pure
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

module Write = struct
  type t =
    { server : Server.t
    ; port : Port.t
    ; username : Username.t
    ; password : Password.t
    ; authentication_method : AuthenticationMethod.t
    ; protocol : Protocol.t
    }
  [@@deriving eq, show]

  let create server port username password authentication_method protocol =
    Ok { server; port; username; password; authentication_method; protocol }
  ;;
end
