module Id = struct
  include Pool_common.Id
end

type t =
  { id : Id.t
  ; model : Entity.Model.t
  ; name : Entity.Name.t
  }
[@@deriving eq, show]

let create ?(id = Id.create ()) model name = { id; model; name }
