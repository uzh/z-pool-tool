open CCFun

module Dynparam = struct
  type t = Pack : 'a Caqti_type.t * 'a -> t

  let empty = Pack (Caqti_type.unit, ())
  let prefix t x (Pack (t', x')) = Pack (Caqti_type.t2 t t', (x, x'))
  let add t x (Pack (t', x')) = Pack (Caqti_type.t2 t' t, (x', x))
end

module Url = struct
  include Pool_model.Base.String

  let field = Pool_message.Field.DatabaseUrl
  let schema () = schema field ()
  let encrypt = Utils.Crypto.String.encrypt_to_string

  let decrypt url =
    let open CCResult in
    url
    |> Utils.Crypto.String.decrypt_from_string
    |> CCResult.map_err (fun _ -> Pool_message.(Error.Decode Field.DatabaseUrl))
    >|= of_string
  ;;

  let to_uri = Uri.of_string
end

module Label = struct
  include Pool_model.Base.String

  let field = Pool_message.Field.DatabaseLabel
  let schema () = schema field ()
  let of_string m = m
  let hash = CCString.hash
end

module Status = struct
  module Core = struct
    let print = Utils.ppx_printer
    let field = Pool_message.Field.Status

    type t =
      | Active [@name "active"] [@printer print "active"]
      | ConnectionIssue [@name "connection_issue"]
      [@printer print "connection_issue"]
      | Disabled [@name "disabled"] [@printer print "disabled"]
      | Maintenance [@name "maintenance"] [@printer print "maintenance"]
      | OpenMigrations [@name "open_migrations"]
      [@printer print "open_migrations"]
    [@@deriving enum, eq, ord, sexp_of, show { with_path = false }, yojson]
  end

  include Pool_model.Base.SelectorType (Core)
  include Core

  let read = Utils.Json.read_variant t_of_yojson

  let of_string str =
    try Ok (read str) with
    | _ -> Error Pool_message.(Error.Invalid Field.Status)
  ;;
end

type t =
  { label : Label.t
  ; url : Url.t [@opaque]
  ; status : Status.t
  }
[@@deriving eq, show, fields]

let create ?(status = Status.Active) label url = { url; label; status }
let root = Label.of_string "root"
let is_root = Label.equal root

type config =
  { url : string
  ; pool_size : int option
  }

let config url pool_size = { url; pool_size }

let schema =
  Conformist.(
    make
      Field.
        [ string
            ~meta:
              "The root database connection url. This is the only string that \
               Sihl needs to connect to a database."
            "DATABASE_URL"
        ; Conformist.optional
            ~meta:
              "The amount of connections in the database connection pool that \
               Sihl manages. If the number is too high, the server might \
               struggle. If the number is too low, your Sihl app performs \
               badly. This can be configured using DATABASE_SIZE and the \
               default is 10."
            (int ~default:10 "DATABASE_SIZE")
        ]
      config)
;;

let database_url () =
  (Sihl.Configuration.read schema).url
  |> Url.create
  |> Pool_message.Error.get_or_failwith
;;

let pool_size () =
  (Sihl.Configuration.read schema).pool_size |> CCOption.value ~default:10
;;

let to_ctx (pool : Label.t) = [ "pool", Label.value pool ]

let of_ctx_opt : (string * string) list -> Label.t option =
  CCList.assoc_opt ~eq:( = ) "pool" %> CCOption.map Label.of_string
;;

let of_ctx_exn =
  of_ctx_opt
  %> CCOption.get_exn_or
       Pool_message.(Error.Undefined Field.DatabaseLabel |> Error.show)
;;
