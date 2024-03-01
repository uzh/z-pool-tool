open Ppx_yojson_conv_lib.Yojson_conv

type t =
  { entity_uuid : Pool_common.Id.t
  ; job : Sihl_queue.instance
  ; message_template : string option
  }
[@@deriving show, fields]

type create =
  { entity_uuids : Pool_common.Id.t list
  ; message_template : string option
  }
[@@deriving show, eq, yojson]

let create ?message_template ~entity_uuid job =
  { entity_uuid; job; message_template }
;;

open Pool_message

let column_created_at =
  (Field.CreatedAt, "pool_message_history.created_at") |> Query.Column.create
;;

let filterable_by = Entity.filterable_by
let searchable_by = Entity.searchable_by
let sortable_by = Entity.sortable_by
let default_sort = Entity.default_sort
let default_query = Entity.default_query
