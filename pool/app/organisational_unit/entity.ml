module Id = Pool_common.Id

module Name = struct
  include Pool_model.Base.String

  let field = Pool_message.Field.Name
  let schema () = schema field ()
end

type t =
  { id : Id.t
  ; name : Name.t
  }
[@@deriving eq, show, fields ~getters, yojson]

let create ?(id = Id.create ()) name = { id; name }

open Pool_message

let column_name =
  (Field.Name, "pool_organisational_units.name") |> Query.Column.create
;;

let column_created_at =
  (Field.CreatedAt, "pool_organisational_units.created_at")
  |> Query.Column.create
;;

let filterable_by = None
let searchable_by = [ column_name ]
let sortable_by = column_created_at :: searchable_by

let default_sort =
  Query.Sort.{ column = column_created_at; order = SortOrder.Descending }
;;

let default_query = Query.create ~sort:default_sort ()
