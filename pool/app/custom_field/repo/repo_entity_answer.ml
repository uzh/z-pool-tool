open Entity_answer
module Common = Pool_common
module Repo = Common.Repo

module Value = struct
  let t = Caqti_type.string
end

module Write = struct
  type t =
    { id : Id.t
    ; custom_field_uuid : Id.t
    ; entity_uuid : Id.t
    ; value : string
    ; version : Pool_common.Version.t
    }
  [@@deriving show, eq]

  let of_entity id custom_field_uuid entity_uuid value version =
    { id; custom_field_uuid; entity_uuid; value; version }
  ;;

  let t =
    let encode (m : t) =
      Ok (m.id, (m.custom_field_uuid, (m.entity_uuid, (m.value, m.version))))
    in
    let decode (id, (custom_field_uuid, (entity_uuid, (value, version)))) =
      Ok { id; custom_field_uuid; entity_uuid; value; version }
    in
    Caqti_type.(
      custom
        ~encode
        ~decode
        (t2 Repo.Id.t (t2 Repo.Id.t (t2 Repo.Id.t (t2 Value.t Repo.Version.t)))))
  ;;
end
