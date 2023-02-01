module Id = struct
  include Pool_common.Id
end

type 'a t =
  { id : Id.t
  ; value : 'a option
  ; admin_value : 'a option
  }
[@@deriving eq, show]

let create ?(id = Id.create ()) ?admin_value value = { id; value; admin_value }
let id { id; _ } = id
let id_opt t = t |> CCOption.map id
let value { value; _ } = value
let admin_value { admin_value; _ } = admin_value
let admin_value_opt t = CCOption.bind t admin_value
