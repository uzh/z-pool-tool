module PoolError = Pool_common.Message

module Url = struct
  include Pool_common.Model.String

  let field = PoolError.Field.DatabaseUrl
  let schema () = schema field ()
end

module Label = struct
  include Pool_common.Model.String

  let field = PoolError.Field.DatabaseLabel
  let schema () = schema field ()
  let of_string m = m
end

type t =
  { url : Url.t
  ; label : Label.t
  }
[@@deriving eq]

let create label url = Ok { url; label }
let root = Label.of_string "root"
let is_root = Label.equal root

module MariaConfig = struct
  include Guardian_backend.Pools.DefaultConfig

  let database =
    Guardian_backend.Pools.MultiPools
      [ ( root |> Label.value
        , Sihl.Configuration.read_string "DATABASE_URL"
          |> CCOption.get_exn_or "DATABASE_URL undefined" )
      ]
  ;;
end

module GuardBackend = Guardian_backend.Pools.Make (MariaConfig)

let add_pool model =
  let pool_size =
    Sihl.Configuration.read_string "DATABASE_POOL_SIZE"
    |> CCFun.flip CCOption.bind CCInt.of_string
    |> CCOption.value ~default:10
  in
  let () = GuardBackend.add_pool ~pool_size model.label model.url in
  Sihl.Database.add_pool ~pool_size model.label model.url
;;

let drop_pool label =
  let label = Label.value label in
  let%lwt () = GuardBackend.drop_pool label in
  Sihl.Database.drop_pool label
;;

let read_pool m = m.label
let pp formatter m = Label.pp formatter m.label
let to_ctx pool = [ "pool", Label.value pool ]

let of_ctx_opt : (string * string) list -> Label.t option =
  let open CCFun in
  CCList.assoc_opt ~eq:( = ) "pool" %> CCOption.map Label.of_string
;;

let of_ctx_exn =
  let open CCFun in
  let open Pool_common in
  of_ctx_opt
  %> CCOption.to_result Message.(Undefined Field.DatabaseLabel)
  %> Utils.get_or_failwith
;;
