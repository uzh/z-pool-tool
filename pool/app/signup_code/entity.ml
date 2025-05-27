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
  type t = int

  let value m = m
end

type t =
  { id : Id.t
  ; code : Code.t
  ; signup_count : Count.t
  ; verification_count : Count.t
  }

let column_code = (Code.field, "pool_signup_codes.code") |> Query.Column.create

let column_signup_count =
  (Field.SignUpCount, "pool_signup_codes.signup_count") |> Query.Column.create
;;

let column_verification_count =
  (Field.VerificationCount, "pool_signup_codes.verification_count") |> Query.Column.create
;;

let filterable_by = None
let searchable_by = [ column_code ]
let sortable_by = [ column_code; column_signup_count; column_verification_count ]
let default_sort = Query.Sort.{ column = column_code; order = SortOrder.Descending }
let default_query = Query.create ~sort:default_sort ()
