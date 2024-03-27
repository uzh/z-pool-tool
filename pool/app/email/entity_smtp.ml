module Id = Pool_common.Id

let print = Utils.ppx_printer

module Label = struct
  include Pool_model.Base.String

  let field = Pool_message.Field.SmtpLabel
  let schema () = schema field ()
end

module Server = struct
  include Pool_model.Base.String

  let field = Pool_message.Field.SmtpServer
  let schema () = schema field ()
end

module Port = struct
  include Pool_model.Base.Integer

  let field = Pool_message.Field.SmtpPort
  let create port = Ok port
  let schema = schema field create
end

module Username = struct
  include Pool_model.Base.String

  let field = Pool_message.Field.SmtpUsername
  let schema () = schema field ()
end

module Password = struct
  include Pool_model.Base.String

  let field = Pool_message.Field.SmtpPassword
  let schema () = schema field ()
  let show _ = "<opaque>"
end

module Mechanism = struct
  module Core = struct
    let field = Pool_message.Field.SmtpMechanism

    (* Open NTLM protocol issue: https://github.com/mirage/colombe/issues/63 *)
    type t =
      | PLAIN [@name "PLAIN"] [@printer print "PLAIN"]
      | LOGIN [@name "LOGIN"] [@printer print "LOGIN"]
    [@@deriving enum, eq, ord, sexp_of, show { with_path = false }, yojson]
  end

  include Pool_model.Base.SelectorType (Core)
  include Core

  let to_sendmail = function
    | PLAIN -> Sendmail.PLAIN
    | LOGIN -> Sendmail.LOGIN
  ;;
end

module Protocol = struct
  module Core = struct
    let field = Pool_message.Field.SmtpProtocol

    type t =
      | STARTTLS [@name "STARTTLS"] [@printer print "STARTTLS"]
      | SSL_TLS [@name "SSL/TLS"] [@printer print "SSL/TLS"]
    [@@deriving enum, eq, ord, sexp_of, show { with_path = false }, yojson]
  end

  include Pool_model.Base.SelectorType (Core)
  include Core
end

module Default = struct
  include Pool_model.Base.Boolean

  let init = false
  let schema = schema Pool_message.Field.DefaultSmtpServer
end

type t =
  { id : Id.t
  ; label : Label.t
  ; server : Server.t
  ; port : Port.t
  ; username : Username.t option [@sexp.option]
  ; mechanism : Mechanism.t
  ; protocol : Protocol.t
  ; default : Default.t
  }
[@@deriving eq, fields ~getters, show, sexp_of]

type smtp = t

type update_password =
  { id : Id.t
  ; password : Password.t option [@opaque]
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
    ; default : Default.t
    }
  [@@deriving eq, show]

  let create ?id label server port username password mechanism protocol default =
    Ok
      { id = id |> CCOption.value ~default:(Id.create ())
      ; label
      ; server
      ; port
      ; username
      ; password
      ; mechanism
      ; protocol
      ; default
      }
  ;;

  let to_entity (t : t) : smtp =
    { id = t.id
    ; label = t.label
    ; server = t.server
    ; port = t.port
    ; username = t.username
    ; mechanism = t.mechanism
    ; protocol = t.protocol
    ; default = t.default
    }
  ;;
end

open Pool_message

let column_label = (Field.Label, "pool_smtp.label") |> Query.Column.create

let column_smtp_server =
  (Field.SmtpServer, "pool_smtp.server") |> Query.Column.create
;;

let column_smtp_username =
  (Field.SmtpUsername, "pool_smtp.username") |> Query.Column.create
;;

let column_smtp_mechanism =
  (Field.SmtpMechanism, "pool_smtp.mechanism") |> Query.Column.create
;;

let column_smtp_protocol =
  (Field.SmtpProtocol, "pool_smtp.protocol") |> Query.Column.create
;;

let column_smtp_default_account =
  (Field.DefaultSmtpServer, "pool_smtp.default_account") |> Query.Column.create
;;

let column_created_at =
  (Field.CreatedAt, "pool_smtp.created_at") |> Query.Column.create
;;

let searchable_by =
  [ column_label
  ; column_smtp_server
  ; column_smtp_username
  ; column_smtp_mechanism
  ; column_smtp_protocol
  ]
;;

let default_sort_column = column_created_at

let sortable_by =
  [ column_created_at; column_smtp_default_account ] @ searchable_by
;;

let default_sort =
  Query.Sort.{ column = default_sort_column; order = SortOrder.Descending }
;;

let filterable_by = None
let default_query = Query.create ~sort:default_sort ()
