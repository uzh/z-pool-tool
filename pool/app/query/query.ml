include Entity

let from_request ?searchable_columns ?sortable_columns req =
  let query_params = Sihl.Web.Request.query_list req in
  let open CCOption in
  let find field =
    CCList.assoc_opt
      ~eq:CCString.equal
      (Pool_common.Message.Field.show field)
      query_params
    >>= CCList.head_opt
    >>= function
    | "" -> None
    | str -> Some str
  in
  let pagination =
    let open Pagination in
    let limit = find Limit.field >>= CCInt.of_string in
    let page = find Page.field >>= CCInt.of_string in
    create ?limit ?page ()
  in
  let search =
    let open Search in
    searchable_columns
    >>= fun columns ->
    find Query.field >|= Query.of_string >|= fun query -> create query columns
  in
  let sort =
    let open Pool_common.Message in
    let order =
      try find Field.SortOrder >|= Sort.SortOrder.read with
      | _ -> None
    in
    sortable_columns
    >>= fun columns ->
    find Field.Order >|= Field.read >>= Sort.create ?order columns
  in
  create ~pagination ?search ?sort ()
;;

let empty () = { pagination = None; search = None; sort = None }

let append_query_to_sql dyn where t =
  let format = Format.asprintf in
  let open CCOption in
  let pagination = t >>= pagination >|= Pagination.to_sql in
  let dyn, search =
    t >>= search |> CCOption.map_or ~default:(dyn, None) (Search.to_sql dyn)
  in
  let order_by =
    t >>= fun { sort; _ } -> sort >|= Sort.to_sql >|= format "ORDER BY %s"
  in
  let paginate_and_sort =
    match order_by, pagination with
    | Some order_by, Some pagination ->
      Some (format "%s %s" order_by pagination)
    | Some str, None | None, Some str -> Some str
    | _ -> None
  in
  let sql =
    match search, where with
    | Some search, Some where -> format "WHERE %s AND %s" where search
    | None, Some where | Some where, None -> format "WHERE %s" where
    | None, None -> ""
  in
  dyn, sql, paginate_and_sort
;;

(* TODO: Add possibility to add additional where *)
let collect_and_count db_pool query ~select ~count caqti_type =
  let open Utils.Database in
  let open Caqti_request.Infix in
  let db_pool = Pool_database.Label.value db_pool in
  let dyn, where, paginate_and_sort =
    append_query_to_sql Dynparam.empty None query
  in
  let (Dynparam.Pack (pt, pv)) = dyn in
  let request =
    let base = select where in
    paginate_and_sort
    |> CCOption.map_or ~default:base (Format.asprintf "%s %s" base)
    |> pt ->* caqti_type
  in
  let count_request = count where |> pt ->! Caqti_type.int in
  let%lwt rows = collect db_pool request pv in
  let%lwt count = find db_pool count_request pv in
  let query = CCOption.value ~default:(empty ()) query in
  Lwt.return (rows, set_page_count query count)
;;
