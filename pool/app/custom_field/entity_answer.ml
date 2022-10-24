module Id = struct
  include Pool_common.Id
end

type 'a t =
  { id : Id.t
  ; value : 'a
  }
[@@deriving eq, show]

let create ?(id = Id.create ()) value = { id; value }
let id { id; _ } = id
let id_opt t = t |> CCOption.map id
