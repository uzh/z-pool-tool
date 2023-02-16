module PoolError = Pool_common.Message
module Id = Pool_common.Id

module Label = struct
  include Pool_common.Model.String

  let field = PoolError.Field.SmtpLabel
  let schema = schema ?validation:None field
end

module Server = struct
  include Pool_common.Model.String

  let field = PoolError.Field.SmtpServer
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
  let show _ = "<opaque>"
end

module Mechanism = struct
  let go m fmt _ = Format.pp_print_string fmt m

  (* Open NTLM protocol issue: https://github.com/mirage/colombe/issues/63 *)
  type t =
    | PLAIN [@name "PLAIN"] [@printer go "PLAIN"]
    | LOGIN [@name "LOGIN"] [@printer go "LOGIN"]
  [@@deriving enum, eq, sexp_of, show { with_path = false }, yojson]

  let read m =
    m
    |> CCString.uppercase_ascii
    |> Format.asprintf "[\"%s\"]"
    |> Yojson.Safe.from_string
    |> t_of_yojson
  ;;

  let all : t list =
    CCList.range min max
    |> CCList.map of_enum
    |> CCList.all_some
    |> CCOption.get_exn_or
         "SMTP auth mechanism: Could not create list of all keys!"
  ;;

  let schema () =
    Pool_common.Utils.schema_decoder
      CCFun.(read %> CCResult.pure)
      show
      PoolError.Field.SmtpMechanism
  ;;

  let to_sendmail = function
    | PLAIN -> Sendmail.PLAIN
    | LOGIN -> Sendmail.LOGIN
  ;;
end

module Protocol = struct
  let go m fmt _ = Format.pp_print_string fmt m

  type t =
    | STARTTLS [@name "STARTTLS"] [@printer go "STARTTLS"]
    | SSL_TLS [@name "SSL/TLS"] [@printer go "SSL/TLS"]
  [@@deriving enum, eq, sexp_of, show { with_path = false }, yojson]

  let read m =
    m
    |> CCString.uppercase_ascii
    |> Format.asprintf "[\"%s\"]"
    |> Yojson.Safe.from_string
    |> t_of_yojson
  ;;

  let all : t list =
    CCList.range min max
    |> CCList.map of_enum
    |> CCList.all_some
    |> CCOption.get_exn_or
         "SMTP auth mechanism: Could not create list of all keys!"
  ;;

  let schema () =
    Pool_common.Utils.schema_decoder
      CCFun.(read %> CCResult.pure)
      show
      PoolError.Field.SmtpProtocol
  ;;
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
