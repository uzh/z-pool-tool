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
end

module Label = struct
  include Pool_model.Base.String

  let field = Pool_message.Field.DatabaseLabel
  let schema () = schema field ()
  let of_string m = m
end

type t =
  { url : Url.t
  ; label : Label.t
  }
[@@deriving eq]

let create label url = { url; label }
let label m = m.label
let pp formatter m = Label.pp formatter m.label
let root = Label.of_string "root"
let is_root = Label.equal root

let root_database_url =
  Sihl.Configuration.read_string "DATABASE_URL"
  |> CCOption.get_exn_or "DATABASE_URL undefined"
;;

module MariaConfigPool = struct
  open Pools
  include DefaultConfig

  let database = MultiPools [ root |> Label.value, root_database_url, true ]
end

module MariaConfig = struct
  open Guardian_backend.Pools
  include DefaultConfig

  let database = MultiPools [ root |> Label.value, root_database_url ]
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
