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

open Pool_common.Message

let column_name =
  (Field.Name, "pool_organisational_units.name") |> Query.Column.create
;;

let column_created_at =
  (Field.CreatedAt, "pool_organisational_units.created_at")
  |> Query.Column.create
;;

let filterable_by = None
let searchable_by = [ column_name ]
let default_sort_column = column_created_at
let sortable_by = default_sort_column :: searchable_by

let default_query =
  let open Query in
  let sort =
    Sort.{ column = default_sort_column; order = SortOrder.Descending }
  in
  create ~sort ()
;;
