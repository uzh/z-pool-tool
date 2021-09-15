module Ql = struct
  module Filter = struct
    type op =
      | Eq
      | Like
      | IsNull

    type criterion =
      { key : string
      ; value : string
      ; op : op
      }

    type t =
      | And of t list
      | Or of t list
      | C of criterion
  end

  module Sort = struct
    type criterion =
      | Asc of string
      | Desc of string

    type t = criterion list

    let criterion_value = function
      | Asc value -> value
      | Desc value -> value
    ;;
  end

  module Limit = struct
    type t = int
  end

  module Offset = struct
    type t = int
  end

  type t =
    { filter : Filter.t option
    ; sort : Sort.t option
    ; limit : Limit.t option
    ; offset : Offset.t option
    }

  module Sql = struct
    let is_field_allowlisted allowlist field =
      allowlist |> List.find_opt (String.equal field) |> Option.is_some
    ;;

    let sort allowlist sort =
      let sorts =
        sort
        |> List.filter (fun criterion ->
               criterion
               |> Sort.criterion_value
               |> is_field_allowlisted allowlist)
        |> List.map (function
               | Sort.Asc value -> Format.asprintf "%s ASC" value
               | Sort.Desc value -> Format.asprintf "%s DESC" value)
        |> String.concat ", "
      in
      if CCString.is_empty sorts
      then ""
      else Format.asprintf "ORDER BY %s" sorts
    ;;

    let filter_criterion_to_string criterion =
      Filter.(
        match criterion.op with
        | Eq -> Format.asprintf "%s %s ?" criterion.key "="
        | Like -> Format.asprintf "%s %s ?" criterion.key "LIKE"
        | IsNull -> Format.asprintf "%s %s" criterion.key "IS NULL")
    ;;

    let is_filter_allowlisted allowlist filter =
      match filter with
      | Filter.C criterion ->
        is_field_allowlisted allowlist Filter.(criterion.key)
      | _ -> true
    ;;

    let filter allowlist filter =
      let values = ref [] in
      let rec to_string filter =
        Filter.(
          match filter with
          | C ({ op = IsNull; _ } as criterion) ->
            filter_criterion_to_string criterion
          | C criterion ->
            values := List.concat [ !values; [ criterion.value ] ];
            filter_criterion_to_string criterion
          | And [] -> ""
          | Or [] -> ""
          | And filters ->
            let allowlisted_filters =
              filters |> List.filter (is_filter_allowlisted allowlist)
            in
            let criterions_string =
              allowlisted_filters |> List.map to_string |> String.concat " AND "
            in
            if List.length allowlisted_filters > 1
            then Format.asprintf "(%s)" criterions_string
            else Format.asprintf "%s" criterions_string
          | Or filters ->
            let allowlisted_filters =
              filters |> List.filter (is_filter_allowlisted allowlist)
            in
            let criterions_string =
              allowlisted_filters |> List.map to_string |> String.concat " OR "
            in
            if List.length allowlisted_filters > 1
            then Format.asprintf "(%s)" criterions_string
            else Format.asprintf "%s" criterions_string)
      in
      let result = to_string filter in
      let result =
        if CCString.is_empty result
        then ""
        else Format.asprintf "WHERE %s" result
      in
      result, !values
    ;;

    let to_fragments field_allowlist query =
      let filter_qs, filter_values =
        query.filter
        |> Option.map (filter field_allowlist)
        |> Option.value ~default:("", [])
      in
      let sort_qs =
        query.sort
        |> Option.map (sort field_allowlist)
        |> Option.value ~default:""
      in
      filter_qs, sort_qs, List.concat [ filter_values ]
    ;;

    let to_string field_allowlist query =
      let filter_fragment, sort_fragment, values =
        to_fragments field_allowlist query
      in
      let qs =
        List.filter
          (fun str -> not (CCString.is_empty str))
          [ filter_fragment; sort_fragment ]
        |> String.concat " "
      in
      qs, values
    ;;
  end

  let to_sql = Sql.to_string
  let to_sql_fragments = Sql.to_fragments
  let empty = { filter = None; sort = None; limit = None; offset = None }
  let set_filter filter query = { query with filter = Some filter }

  let set_filter_and criterion query =
    let open Filter in
    let new_filter =
      match query.filter with
      | Some filter -> And (List.append [ filter ] [ C criterion ])
      | None -> C criterion
    in
    { query with filter = Some new_filter }
  ;;

  let set_sort sort query = { query with sort = Some sort }
end
