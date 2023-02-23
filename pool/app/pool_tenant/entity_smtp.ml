module PoolError = Pool_common.Message
module Id = Pool_common.Id

let print = Utils.ppx_printer

module Label = struct
  include Pool_common.Model.String

  let field = PoolError.Field.SmtpLabel
  let schema () = schema field ()
end

module Server = struct
  include Pool_common.Model.String

  let field = PoolError.Field.SmtpServer
  let schema () = schema field ()
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
  let schema () = schema field ()
end

module Password = struct
  include Pool_common.Model.String

  let field = PoolError.Field.SmtpPassword
  let schema () = schema field ()
  let show _ = "<opaque>"
end

module Mechanism = struct
  module Core = struct
    let field = Pool_common.Message.Field.SmtpMechanism

    (* Open NTLM protocol issue: https://github.com/mirage/colombe/issues/63 *)
    type t =
      | PLAIN [@name "PLAIN"] [@printer print "PLAIN"]
      | LOGIN [@name "LOGIN"] [@printer print "LOGIN"]
    [@@deriving enum, eq, ord, sexp_of, show { with_path = false }, yojson]
  end

  include Pool_common.Model.SelectorType (Core)
  include Core

  let to_sendmail = function
    | PLAIN -> Sendmail.PLAIN
    | LOGIN -> Sendmail.LOGIN
  ;;
end

module Protocol = struct
  module Core = struct
    let field = Pool_common.Message.Field.SmtpProtocol

    type t =
      | STARTTLS [@name "STARTTLS"] [@printer print "STARTTLS"]
      | SSL_TLS [@name "SSL/TLS"] [@printer print "SSL/TLS"]
    [@@deriving enum, eq, ord, sexp_of, show { with_path = false }, yojson]
  end

  include Pool_common.Model.SelectorType (Core)
  include Core
end

type t =
  { id : Id.t
  ; label : Label.t
  ; server : Server.t
  ; port : Port.t
  ; username : Username.t option [@sexp.option]
  ; mechanism : Mechanism.t
  ; protocol : Protocol.t
  }
[@@deriving eq, show, sexp_of]

type update_password =
  { id : Id.t
  ; password : Password.t option
  }
[@@deriving eq, show]

module Write = struct
  type t =
    { id : Id.t
    ; label : Label.t
    ; server : Server.t
    ; port : Port.t
    ; username : Username.t option
    ; password : Password.t option [@opaque]
    ; mechanism : Mechanism.t
    ; protocol : Protocol.t
    }
  [@@deriving eq, show]

  let create ?id label server port username password mechanism protocol =
    Ok
      { id = id |> CCOption.value ~default:(Id.create ())
      ; label
      ; server
      ; port
      ; username
      ; password
      ; mechanism
      ; protocol
      }
  ;;
end
