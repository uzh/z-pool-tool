module PoolError = Pool_common.Message

module Url = struct
  type t = string [@@deriving eq]

  let create url =
    if CCString.is_empty url
    then Error PoolError.(Invalid DatabaseUrl)
    else Ok url
  ;;

  let value m = m

  let schema () =
    Pool_common.Utils.schema_decoder create value PoolError.DatabaseUrl
  ;;
end

module Label = struct
  open Sexplib.Conv

  type t = string [@@deriving eq, show, sexp_of]

  let value m = m
  let of_string m = m

  let create label =
    if CCString.is_empty label || String.contains label ' '
    then Error PoolError.(Invalid DatabaseLabel)
    else Ok label
  ;;

  let schema () =
    Pool_common.Utils.schema_decoder create value PoolError.DatabaseLabel
  ;;
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
