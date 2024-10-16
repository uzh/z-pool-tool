let url_key = Pool_message.Field.SignUpCode

module Field = Pool_message.Field

module Id = struct
  include Pool_model.Base.Id
end

module Code = struct
  include Pool_model.Base.String

  let field = Field.Code
  let schema = schema field ~validation:create
end

module Count = struct
  include Pool_model.Base.Integer

  let create = CCResult.return
  let field = Field.Count
  let schema = schema field create
end

type t =
  { id : Id.t
  ; code : Code.t
  ; count : Count.t
  }

let column_code = (Code.field, "pool_signup_codes.code") |> Query.Column.create

let column_count =
  (Count.field, "pool_signup_codes.count") |> Query.Column.create
;;

let filterable_by = None
let searchable_by = [ column_code ]
let sortable_by = [ column_code; column_count ]

let default_sort =
  Query.Sort.{ column = column_code; order = SortOrder.Descending }
;;

let default_query = Query.create ~sort:default_sort ()
