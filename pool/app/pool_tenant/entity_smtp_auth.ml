module PoolError = Pool_common.Message

module Server = struct
  include Pool_common.Model.String

  let field = PoolError.Field.SmtpAuthServer
  let schema = schema ?validation:None field
end

module Port = struct
  include Pool_common.Model.Integer

  let field = PoolError.Field.SmtpPort
  let create port = Ok port
  let schema = schema field create
end

module Username = struct
  include Pool_common.Model.String

  let field = PoolError.Field.SmtpUsername
  let schema = schema ?validation:None field
end

module Password = struct
  include Pool_common.Model.String

  let field = PoolError.Field.SmtpPassword
  let schema = schema ?validation:None field
  let show m = CCString.repeat "*" @@ CCString.length m
end

module AuthenticationMethod = struct
  include Pool_common.Model.String

  let field = PoolError.Field.SmtpAuthMethod
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
  ; username : Username.t option [@sexp.option]
  ; authentication_method : AuthenticationMethod.t
  ; protocol : Protocol.t
  }
[@@deriving eq, show, sexp_of]

module Write = struct
  type t =
    { server : Server.t
    ; port : Port.t
    ; username : Username.t option
    ; password : Password.t option [@opaque]
    ; authentication_method : AuthenticationMethod.t
    ; protocol : Protocol.t
    }
  [@@deriving eq, show]

  let create server port username password authentication_method protocol =
    Ok { server; port; username; password; authentication_method; protocol }
  ;;
end
