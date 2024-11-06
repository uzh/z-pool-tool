module Field = Pool_message.Field

(* The script will determine a score between 0 and 1 based on how equal two
   users are. The alert_threshold is the value from which it will be seen as a
   possible duplicate *)

let alert_threshold = 0.7

module Id = struct
  include Pool_model.Base.Id
end

module Ignored = struct
  include Pool_model.Base.Boolean
end

module SimilarityCriteria = struct
  type t =
    | Fuzzy
    | Exact
  [@@deriving eq, show]
end

module Column = struct
  type t =
    { field : Field.t
    ; criteria : SimilarityCriteria.t
    ; sql_column : string
    ; sql_table : string
    ; weight : int
    }
  [@@deriving eq, show]
end

let columns =
  [ Field.Name, SimilarityCriteria.Exact, "user_users", "name", 5
  ; Field.Firstname, SimilarityCriteria.Exact, "user_users", "given_name", 4
  ; Field.CellPhone, SimilarityCriteria.Exact, "pool_contacts", "cell_phone", 3
  ]
  |> CCList.map (fun (field, criteria, sql_table, sql_column, weight) ->
    { Column.field; criteria; sql_table; sql_column; weight })
;;

type t =
  { id : Id.t
  ; contact_a : Contact.t
  ; contact_b : Contact.t
  ; score : float
  ; ignored : Ignored.t
  }
[@@deriving eq, show]

open Query
open Pool_message

let searchable_by = []
let sortable_by = []

let column_ignore =
  Column.create
    (Field.HideIgnored, "pool_contacts_possible_duplicates.ignore = 0")
;;

let column_score =
  Column.create (Field.Score, "pool_contacts_possible_duplicates.score")
;;

let filterable_by = Some Filter.Condition.Human.[ Checkbox column_ignore ]

let default_query =
  create
    ~sort:Sort.{ column = column_score; order = SortOrder.Descending }
    ~filter:Filter.[ Condition.(Checkbox (column_ignore, true)) ]
    ()
;;
