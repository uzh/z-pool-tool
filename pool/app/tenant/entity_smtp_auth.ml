module PoolError = Pool_common.Message

module Server = struct
  type t = string [@@deriving eq, show]

  let value m = m

  let create server =
    if CCString.is_empty server
    then Error PoolError.(Invalid SmtpAuthServer)
    else Ok server
  ;;

  let schema () =
    Pool_common.Utils.schema_decoder create value PoolError.SmtpAuthServer
  ;;
end

module Port = struct
  type t = string [@@deriving eq, show]

  let value m = m

  let create port =
    if CCString.is_empty port && CCInt.of_string port |> CCOption.is_none
    then Error PoolError.(Invalid SmtpPort)
    else Ok port
  ;;

  let schema () =
    Pool_common.Utils.schema_decoder create value PoolError.SmtpPort
  ;;
end

module Username = struct
  type t = string [@@deriving eq, show]

  let value m = m

  let create username =
    if CCString.is_empty username
    then Error PoolError.(Invalid SmtpUsername)
    else Ok username
  ;;

  let schema () =
    Pool_common.Utils.schema_decoder create value PoolError.SmtpUsername
  ;;
end

module Password = struct
  type t = string [@@deriving eq, show]

  let show m = CCString.repeat "*" @@ CCString.length m

  let create password =
    if CCString.is_empty password
    then Error PoolError.(Invalid SmtpPassword)
    else Ok password
  ;;

  let schema () =
    Pool_common.Utils.schema_decoder create show PoolError.SmtpPassword
  ;;
end

module AuthenticationMethod = struct
  type t = string [@@deriving eq, show]

  let value m = m

  let create authentication_method =
    if CCString.is_empty authentication_method
    then Error PoolError.(Invalid SmtpAuthMethod)
    else Ok authentication_method
  ;;

  let schema () =
    Pool_common.Utils.schema_decoder create value PoolError.SmtpAuthMethod
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
    Pool_common.Utils.schema_decoder create value PoolError.SmtpProtocol
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
