module Record = struct
  include Changelog.DefaultSettings
  open Entity

  let changelog_compare_at_index_keys = Some [ "and"; "or" ]

  type t = Entity.t

  (* This function provides a more human readable json representation than the
     yojson_ppx *)
  let yojson_of_t { id; query; title; created_at; updated_at } =
    let open Utils.Ptime in
    let open Pool_common in
    let predicate_key : Key.t -> string =
      let open Key in
      function
      | CustomField id -> Custom_field.Id.value id
      | Hardcoded key -> show_hardcoded key
    in
    let yojson_of_predicate ({ Predicate.operator; value; _ } : Predicate.t) =
      let yojson_of_value = function
        | NoValue -> `Null
        | Single single -> single |> yojson_of_single_val
        | Lst values -> `List (CCList.map yojson_of_single_val values)
      in
      let operator = Operator.yojson_of_t operator in
      let value = yojson_of_value value in
      `Assoc Helper.[ operator_string, operator; value_string, value ]
    in
    let rec yojson_of_query f : Yojson.Safe.t =
      (match f with
       | And queries -> show_query f, `List (CCList.map yojson_of_query queries)
       | Or queries -> show_query f, `List (CCList.map yojson_of_query queries)
       | Not f -> show_query f, f |> yojson_of_query
       | Pred p ->
         let open Predicate in
         predicate_key p.key, yojson_of_predicate p
       | Template id -> show_query f, `String (Id.value id))
      |> fun (key, pred) -> `Assoc [ key, pred ]
    in
    `Assoc
      [ "id", `String (Pool_common.Id.value id)
      ; "query", yojson_of_query query
      ; "title", `String (title |> Option.value ~default:"")
      ; "created_at", CreatedAt.value created_at |> yojson_of_ptime
      ; "updated_at", UpdatedAt.value updated_at |> yojson_of_ptime
      ]
  ;;

  let model = Pool_message.Field.Filter
end

include Changelog.T (Record)
