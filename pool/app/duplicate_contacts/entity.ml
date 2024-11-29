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

type hardcoded =
  | Lastname of Pool_user.Lastname.t
  | Firstname of Pool_user.Firstname.t
  | EmailAddress of Pool_user.EmailAddress.t
  | CellPhone of Pool_user.CellPhone.t option
  | Language of Pool_common.Language.t option
[@@deriving eq, show, variants]

let hardcoded_fields =
  [ Field.Lastname
  ; Field.Firstname
  ; Field.EmailAddress
  ; Field.CellPhone
  ; Field.Language
  ]
;;

let read_hardcoded =
  let open CCFun.Infix in
  [ Field.Lastname, Contact.lastname %> lastname
  ; Field.Firstname, Contact.firstname %> firstname
  ; Field.EmailAddress, Contact.email_address %> emailaddress
  ; Field.CellPhone, Contact.cell_phone %> cellphone
  ; (Field.Language, fun c -> language c.Contact.language)
  ]
;;

type merge =
  { contact : Contact.t
  ; merged_contact : Contact.t
  ; kept_fields : Custom_field.Public.t list
  }

let searchable_by = []

let column_ignore =
  Column.create
    (Field.HideIgnored, "pool_contacts_possible_duplicates.ignore = 0")
;;

let column_score =
  Column.create (Field.Score, "pool_contacts_possible_duplicates.score")
;;

let filterable_by = Some Filter.Condition.Human.[ Checkbox column_ignore ]
let sortable_by = [ column_score ]

let default_query =
  create
    ~sort:Sort.{ column = column_score; order = SortOrder.Descending }
    ~filter:Filter.[ Condition.(Checkbox (column_ignore, true)) ]
    ()
;;
