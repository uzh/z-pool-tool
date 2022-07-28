module PoolError = Pool_common.Message

module Server = struct
  include Pool_common.Model.String

  let field = PoolError.Field.SmtpAuthServer
  let create = create
  let schema = schema ?validation:None field
end

module Port = struct
  include Pool_common.Model.String

  let field = PoolError.Field.SmtpPort

  let create port =
    if CCString.is_empty port && CCInt.of_string port |> CCOption.is_none
    then Error PoolError.(Invalid field)
    else Ok port
  ;;

  let schema = schema ~validation:create field
end

module Username = struct
  include Pool_common.Model.String

  let field = PoolError.Field.SmtpUsername
  let create = create
  let schema = schema ?validation:None field
end

module Password = struct
  include Pool_common.Model.String

  let field = PoolError.Field.SmtpPassword
  let create = create
  let schema = schema ?validation:None field
  let show m = CCString.repeat "*" @@ CCString.length m
end

module AuthenticationMethod = struct
  include Pool_common.Model.String

  let field = PoolError.Field.SmtpAuthMethod
  let create = create
  let schema = schema ?validation:None field
  let show m = CCString.repeat "*" @@ CCString.length m
end

module Protocol = struct
  include Pool_common.Model.String

  let field = PoolError.Field.SmtpProtocol

  let create protocol =
    if CCList.mem protocol [ "STARTTLS"; "SSL/TLS" ]
    then Ok protocol
    else Error PoolError.(Invalid field)
  ;;

  let schema = schema ~validation:create field
end

type t =
  { server : Server.t
  ; port : Port.t
  ; username : Username.t
  ; authentication_method : AuthenticationMethod.t
  ; protocol : Protocol.t
  }
[@@deriving eq, show, sexp_of]

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
