open Entity

type t =
  { entity_uuid : Pool_common.Id.t
  ; queue_uuid : Id.t
  ; entity : History.model
  }
[@@deriving eq, show, fields]

let create { Instance.id; _ } (entity, entity_uuid) =
  { entity_uuid; queue_uuid = id; entity }
;;
