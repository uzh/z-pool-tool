module Id = struct
  include Pool_common.Id
end

type 'a t =
  { id : Id.t
  ; entity_uuid : Pool_common.Id.t
  ; value : 'a option
  ; admin_value : 'a option
  }
[@@deriving eq, show]

let create ?(id = Id.create ()) ?admin_value entity_uuid value =
  { id; entity_uuid; value; admin_value }
;;

let id { id; _ } = id
let id_opt t = t |> CCOption.map id
let value { value; _ } = value
let admin_value { admin_value; _ } = admin_value
let admin_value_opt t = CCOption.bind t admin_value
let entity_uuid { entity_uuid; _ } = entity_uuid

let equal_value a b =
  match a, b with
  | Some a, Some b ->
    (match a.value, b.value with
     | Some a, Some b -> a = b
     | None, None -> true
     | _ -> false)
  | _, _ -> false
;;
