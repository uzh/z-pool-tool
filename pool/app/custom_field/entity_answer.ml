module Id = struct
  include Pool_common.Id
end

type 'a t =
  { id : Id.t
  ; value : 'a
  ; overridden_value : 'a option
  }
[@@deriving eq, show]

let create ?(id = Id.create ()) ?overridden_value value =
  { id; value; overridden_value }
;;

let id { id; _ } = id
let id_opt t = t |> CCOption.map id
