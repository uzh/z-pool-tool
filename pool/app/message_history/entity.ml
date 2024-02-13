open Ppx_yojson_conv_lib.Yojson_conv

type t =
  { entity_uuid : Pool_common.Id.t
  ; job : Sihl_queue.instance
  ; message_template : string option
  (* Should I pass a UUID? entity_specific templates can be deleted *)
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

open Pool_common.Message

let column_created_at =
  (Field.CreatedAt, "pool_message_history.created_at") |> Query.Column.create
;;

let filterable_by = None
let searchable_by = []
let sortable_by = [ column_created_at ]

let default_sort =
  Query.Sort.{ column = column_created_at; order = SortOrder.Descending }
;;

let default_query = Query.create ~sort:default_sort ()
