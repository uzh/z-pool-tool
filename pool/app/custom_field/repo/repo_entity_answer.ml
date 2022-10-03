open Entity_answer
module Common = Pool_common
module Repo = Common.Repo

module Value = struct
  let t = Caqti_type.string
end

type repo =
  { id : Id.t
  ; value : string
  ; version : Common.Version.t
  }
[@@deriving eq, show]

let t =
  let encode (m : repo) = Ok (m.id, (m.value, m.version)) in
  let decode (id, (value, version)) = Ok ({ id; value; version } : repo) in
  Caqti_type.(
    custom ~encode ~decode (tup2 Repo.Id.t (tup2 Value.t Repo.Version.t)))
;;

module Write = struct
  type t =
    { id : Id.t
    ; custom_field_uuid : Id.t
    ; entity_uuid : Id.t
    ; value : string
    ; version : Common.Version.t
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
        (tup2
           Repo.Id.t
           (tup2 Repo.Id.t (tup2 Repo.Id.t (tup2 Value.t Repo.Version.t)))))
  ;;
end
