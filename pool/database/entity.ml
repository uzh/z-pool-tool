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
end

module Label = struct
  include Pool_model.Base.String

  let field = Pool_message.Field.DatabaseLabel
  let schema () = schema field ()
  let of_string m = m
end

module Disabled = struct
  include Pool_model.Base.Boolean

  let field = Pool_message.Field.Disabled
  let schema () = schema field ()
end

type t =
  { label : Label.t
  ; url : Url.t [@opaque]
  ; disabled : Disabled.t
  }
[@@deriving eq, show, fields]

let create ?(disabled = false) label url = { url; label; disabled }
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

module MariaConfigPool = struct
  open Pools
  include DefaultConfig

  let database = MultiPools [ root |> Label.value, database_url (), true ]
end

module MariaConfig = struct
  open Guardian_backend.Pools
  include DefaultConfig

  let database = MultiPools [ root |> Label.value, database_url () ]
end

let to_ctx (pool : Label.t) = [ "pool", Label.value pool ]

let of_ctx_opt : (string * string) list -> Label.t option =
  CCList.assoc_opt ~eq:( = ) "pool" %> CCOption.map Label.of_string
;;

let of_ctx_exn =
  of_ctx_opt
  %> CCOption.get_exn_or
       Pool_message.(Error.Undefined Field.DatabaseLabel |> Error.show)
;;
