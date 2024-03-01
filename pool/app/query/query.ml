include Entity

let from_request
  ?(filterable_by : Filter.human option)
  ?(searchable_by : Column.t list option)
  ?(sortable_by : Column.t list option)
  ?(default : t option)
  req
  =
  let query_params = Sihl.Web.Request.query_list req in
  let open CCOption in
  let find field =
    CCList.assoc_opt
      ~eq:CCString.equal
      (Pool_message.Field.show field)
      query_params
    >>= CCList.head_opt
    >>= function
    | "" -> None
    | str -> Some str
  in
  let filter =
    let open Filter in
    let open Condition in
    let open CCOption in
    let find_column col = col |> Column.field |> find in
    filterable_by
    >>= fun filterable_by ->
    filterable_by
    |> CCList.filter_map (function
      | Human.Checkbox col ->
        let value = find_column col in
        value >>= Utils.Bool.of_string_opt >|= checkbox col
      | Human.Select (col, options) ->
        let value = find_column col in
        value >>= SelectOption.find_by_value options >|= select col)
    |> function
    | [] -> None
    | conditions -> Some conditions
  in
  let pagination =
    let open Pagination in
    let limit = find Limit.field >>= CCInt.of_string in
    let page = find Page.field >>= CCInt.of_string in
    create ?limit ?page ()
  in
  let search =
    let open Search in
    searchable_by
    >>= fun columns ->
    find Query.field >|= Query.of_string >|= fun query -> create query columns
  in
  let sort =
    let open Pool_message in
    let open Pool_common in
    let order =
      try find Field.SortOrder >|= Sort.SortOrder.read with
      | Yojson.Json_error exn ->
        Utils.handle_json_parse_err exn |> CCFun.const None
      | Ppx_yojson_conv_lib.Yojson_conv.Of_yojson_error (exn, yojson) ->
        Utils.handle_ppx_yojson_err (exn, yojson) |> CCFun.const None
    in
    sortable_by
    >>= fun columns ->
    find Field.Order >|= Field.read >>= Sort.create ?order columns
  in
  create ~pagination ?filter ?search ?sort () |> apply_default ~default
;;

let empty () = create ()

let append_query_to_sql dyn where t =
  let format = Format.asprintf in
  let open CCOption in
  let pagination = t >>= pagination >|= Pagination.to_sql in
  let dyn, where =
    t
    >>= (fun { filter; _ } -> filter >|= Filter.to_sql ?where dyn)
    |> value ~default:(dyn, where)
  in
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
    | None, None -> None
  in
  let sql =
    match search, where with
    | Some search, Some where -> format "WHERE %s AND (%s)" where search
    | None, Some where | Some where, None -> format "WHERE %s" where
    | None, None -> ""
  in
  dyn, sql, paginate_and_sort
;;

let collect_and_count
  database_label
  query
  ~(select : ?count:bool -> string -> string)
  ?where
  caqti_type
  =
  let open Utils.Database in
  let open Caqti_request.Infix in
  let database_label = Pool_database.Label.value database_label in
  let where, dyn =
    CCOption.map_or
      ~default:(None, Dynparam.empty)
      (fun (where, dyn) -> Some where, dyn)
      where
  in
  let Dynparam.Pack (pt, pv), where, paginate_and_sort =
    append_query_to_sql dyn where query
  in
  let request =
    let base = select where in
    paginate_and_sort
    |> CCOption.map_or ~default:base (Format.asprintf "%s %s" base)
    |> pt ->* caqti_type
  in
  let count_request = select ~count:true where |> pt ->! Caqti_type.int in
  let%lwt rows = collect database_label request pv in
  let%lwt count = find database_label count_request pv in
  let query = CCOption.value ~default:(empty ()) query in
  Lwt.return (rows, set_page_count query count)
;;
