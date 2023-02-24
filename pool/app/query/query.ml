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

let append_query_to_sql dyn where t =
  let open CCOption in
  let pagination = t >>= pagination >|= Pagination.to_sql in
  let dyn, search =
    t >>= search |> CCOption.map_or ~default:(dyn, None) (Search.to_sql dyn)
  in
  let sql =
    match search, where with
    | Some search, Some where -> Format.asprintf "WHERE %s AND %s" where search
    | None, Some where | Some where, None -> Format.asprintf "WHERE %s" where
    | None, None -> ""
  in
  dyn, sql, pagination
;;

(* TODO: Add possibility to add additional where *)
let collect_and_count db_pool query ~select ~count caqti_type =
  let open Utils.Database in
  let open Caqti_request.Infix in
  let db_pool = Pool_database.Label.value db_pool in
  let dyn, where, pagination = append_query_to_sql Dynparam.empty None query in
  let (Dynparam.Pack (pt, pv)) = dyn in
  let request =
    let base = select where in
    pagination
    |> CCOption.map_or ~default:base (Format.asprintf "%s %s" base)
    |> pt ->* caqti_type
  in
  let count_request = count where |> pt ->! Caqti_type.int in
  let%lwt rows = collect db_pool request pv in
  let%lwt count = find db_pool count_request pv in
  let query = CCOption.value ~default:(empty ()) query in
  Lwt.return (rows, set_page_count query count)
;;
