include Entity

let from_request ?searchable_columns req =
  let query_params = Sihl.Web.Request.query_list req in
  let open CCOption in
  let find field =
    CCList.assoc_opt
      ~eq:CCString.equal
      (Pool_common.Message.Field.show field)
      query_params
    >>= CCList.head_opt
  in
  let pagination =
    let open Pagination in
    let limit = find Limit.field >>= CCInt.of_string in
    let page = find Page.field >>= CCInt.of_string in
    create ?limit ?page ()
  in
  let search =
    let open Search in
    let open CCOption in
    searchable_columns
    >>= fun columns ->
    find Query.field >|= Query.of_string >|= fun query -> create query columns
  in
  create ~pagination ?search ()
;;

let empty () = { pagination = None; search = None }

let append_query_to_sql (dyn, sql) where t =
  let open CCOption in
  let pagination = t >>= pagination >|= Pagination.to_sql in
  let dyn, search =
    t >>= search |> CCOption.map_or ~default:(dyn, None) (Search.to_sql dyn)
  in
  let sql =
    match search, where with
    | Some search, Some where ->
      Format.asprintf "%s WHERE %s AND %s" sql where search
    | None, Some where | Some where, None ->
      Format.asprintf "%s WHERE %s" sql where
    | None, None -> sql
  in
  dyn, sql, pagination
;;
