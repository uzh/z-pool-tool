open Entity

type t =
  { entity_uuid : Pool_common.Id.t
  ; queue_uuid : Id.t
  ; entity : History.model
  }
[@@deriving eq, show, fields]

let create { Instance.id; _ } ((entity, entity_uuid) : History.item) =
  { entity_uuid; queue_uuid = id; entity }
;;

open Pool_message

let column_created_at =
  (Field.CreatedAt, "pool_queue_jobs_mapping.created_at") |> Query.Column.create
;;
