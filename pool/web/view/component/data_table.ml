open Tyxml.Html

let hx_get ~url ~target =
  [ a_user_data "hx-get" url
  ; a_user_data "hx-push-url" url
  ; a_user_data "hx-target" target
  ; a_user_data "hx-swap" "innerHTML"
  ]
;;

type sort =
  { url : Uri.t
  ; query : Query.t
  ; language : Pool_common.Language.t
  ; search : Query.Column.t list option
  }

type col =
  [ `column of Query.Column.t
  | `custom of [ | Html_types.flow5 ] Tyxml_html.elt
  | `field of Pool_common.Message.Field.t * Query.Column.t
  | `empty
  ]

let is_selected { query; _ } column =
  let open Query in
  let field = Column.field column in
  match query.sort with
  | Some sort -> Column.field Sort.(sort.column) = field
  | None -> false
;;

let direction ({ query; _ } as sort) column =
  let open Query in
  let direction =
    let open Query.Sort in
    match query.sort with
    | Some sort -> Sort.(sort.order)
    | None -> SortOrder.Ascending
  in
  if is_selected sort column
  then Query.Sort.SortOrder.flip direction
  else direction
;;

let make_url ({ url; query; _ } as sort) column =
  let query =
    query
    |> Query.with_sort_order (direction sort column)
    |> Query.with_sort_column column
    |> Query.to_uri_query
  in
  let url = Uri.with_query url query in
  Format.asprintf "%a" Uri.pp url
;;

let make_name { language; _ } field =
  span [ Component_table.field_to_txt language field ]
;;

let sort_icon sort col =
  let icon =
    let open Query.Sort.SortOrder in
    match direction sort col with
    | Ascending -> Component_icon.CaretUp
    | Descending -> Component_icon.CaretDown
  in
  Component_icon.to_html icon
;;

let make_sortable_head target_id sort col field =
  let url = make_url sort col |> Sihl.Web.externalize_path in
  span
    ~a:
      (hx_get ~url ~target:("#" ^ target_id)
       @ [ a_class [ "has-icon"; "pointer" ] ])
    (if is_selected sort col
     then [ make_name sort field; sort_icon sort col ]
     else [ make_name sort field ])
;;

let make_head target_id sort column =
  th
    [ (match column with
       | `custom el -> el
       | `empty -> txt ""
       | `field (field, col) -> make_sortable_head target_id sort col field
       | `column col ->
         make_sortable_head target_id sort col (Query.Column.field col))
    ]
;;

let make_header target_id cols sort =
  thead [ tr (CCList.map (make_head target_id sort) cols) ]
;;

let make
  ?(align_last_end = true)
  ?align_top
  ?(layout = `Striped)
  ~target_id
  ~cols
  ~row
  sort
  items
  =
  let default = txt "" in
  let search_bar =
    sort.search
    |> CCOption.map_or
         ~default
         (Component_list.searchbar
            ~url:sort.url
            ~target_id
            sort.language
            sort.query)
  in
  let pagination =
    sort.query.Query.pagination
    |> CCOption.map_or
         ~default
         (Component_list.pagination sort.language sort.query)
  in
  let thead = make_header target_id cols sort in
  let rows = CCList.map row items in
  let classes =
    a_class (Component_table.table_classes ?align_top layout ~align_last_end ())
  in
  div
    ~a:[ a_class [ "stack" ]; a_id target_id ]
    [ search_bar; table ~a:[ classes ] ~thead rows; pagination ]
;;
