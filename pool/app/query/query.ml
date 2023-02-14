include Entity

let from_request req =
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
  create ~pagination ()
;;

let empty () = { pagination = None }

let append_pagination_to_sql t sql =
  let open CCFun in
  CCOption.bind t pagination
  |> CCOption.map_or
       ~default:sql
       (Pagination.query_to_sql %> Format.asprintf "%s %s" sql)
;;
