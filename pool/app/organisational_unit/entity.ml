module Id = Pool_common.Id

module Name = struct
  include Pool_common.Model.String

  let field = Pool_common.Message.Field.Name
  let schema () = schema field ()
end

type t =
  { id : Id.t
  ; name : Name.t
  }
[@@deriving eq, show]

let create ?(id = Id.create ()) name = { id; name }
