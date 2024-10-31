module Field = Pool_message.Field

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
    }
  [@@deriving eq, show]
end

let columns =
  [ Field.Name, SimilarityCriteria.Fuzzy, "user_users", "name"
  ; Field.Firstname, SimilarityCriteria.Fuzzy, "user_users", "given_name"
  ]
  |> CCList.map (fun (field, criteria, sql_table, sql_column) ->
    { Column.field; criteria; sql_table; sql_column })
;;

type t =
  { target_user_uuid : Pool_common.Id.t
  ; user_uuid : Pool_common.Id.t
  ; email : Pool_user.EmailAddress.t
  ; similarity_score : float
  }
[@@deriving eq, show]
