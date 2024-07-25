type t =
  { entity_uuid : Pool_common.Id.t
  ; job : Entity.Instance.t
  }
[@@deriving eq, show, fields]

let create job entity_uuid = { entity_uuid; job }

module Write = struct
  type t =
    { queue_uuid : Entity.Id.t
    ; entity_uuid : Pool_common.Id.t
    }
  [@@deriving show]

  let create queue_uuid entity_uuid = { queue_uuid; entity_uuid }
end

let to_write { entity_uuid; job } =
  { Write.entity_uuid; queue_uuid = job.Entity.Instance.id }
;;

open Pool_message

let column_created_at =
  (Field.CreatedAt, "pool_queue_jobs_mapping.created_at") |> Query.Column.create
;;
