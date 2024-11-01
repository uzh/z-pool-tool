module Field = Pool_message.Field

module Id = struct
  include Pool_model.Base.Id
end

(** EXACT: an exact macth is definitely required when comparing custom field
    answers with select options

    PARTIAL: partial match can be used to compare 'firstname' with 'firstname
    middlename'

    FUZZY:
    - https://en.wikipedia.org/wiki/Levenshtein_distance
    - https://github.com/juanmirocks/Levenshtein-MySQL-UDF
    - https://mariadb.com/kb/en/soundex/
    - https://mariadb.com/kb/en/match-against/

    GENERAL QUESTIONS:
    - comparison of null and some value, e.g. Some phone and no phone *)
module SimilarityCriteria = struct
  type t =
    | Fuzzy
    (* | Partial *)
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
  ; target_contact_id : Contact.Id.t
  ; contact : Contact.t
  ; score : float
  }
[@@deriving eq, show]
