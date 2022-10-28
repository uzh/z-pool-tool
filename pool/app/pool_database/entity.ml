module PoolError = Pool_common.Message

module Url = struct
  include Pool_common.Model.String

  let field = PoolError.Field.DatabaseUrl
  let create = create field
  let schema = schema ?validation:None field
end

module Label = struct
  include Pool_common.Model.String

  let field = PoolError.Field.DatabaseLabel
  let create = create field
  let schema = schema ?validation:None field
  let of_string m = m
end

type t =
  { url : Url.t
  ; label : Label.t
  }
[@@deriving eq]

let create url label = Ok { url; label }

let add_pool model =
  Sihl.Database.add_pool
    ~pool_size:
      (Sihl.Configuration.read_string "DATABASE_POOL_SIZE"
      |> CCFun.flip CCOption.bind CCInt.of_string
      |> CCOption.value ~default:10)
    model.label
    model.url
;;

let read_pool m = m.label
let pp formatter m = Label.pp formatter m.label
