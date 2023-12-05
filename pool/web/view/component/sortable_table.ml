open Tyxml.Html

let hx_get ~url ~target =
  [ a_user_data "hx-get" url
  ; a_user_data "hx-push-url" url
  ; a_user_data "hx-target" target
  ; a_user_data "hx-swap" "outerHTML"
  ]
;;

type sort =
  { url : Uri.t
  ; query : Query.t
  ; language : Pool_common.Language.t
  }

type col =
  [ `column of Query.Column.t
  | `custom of [ | Html_types.flow5 ] Tyxml_html.elt
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

let make_name { language; _ } col =
  span [ Component_table.field_to_txt language (Query.Column.field col) ]
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

let make_sortable_head id sort col =
  let url = make_url sort col in
  span
    ~a:(hx_get ~url ~target:("#" ^ id) @ [ a_class [ "has-icon"; "pointer" ] ])
    (if is_selected sort col
     then [ make_name sort col; sort_icon sort col ]
     else [ make_name sort col ])
;;

let make_head id sort column =
  th
    [ (match column with
       | `custom el -> el
       | `column col -> make_sortable_head id sort col)
    ]
;;

let make_header id cols sort =
  thead [ tr (CCList.map (make_head id sort) cols) ]
;;

let make_rows rows =
  CCList.map (fun row -> tr (CCList.map (fun cell -> td [ cell ]) row)) rows
;;

let make ?(layout = `Striped) ?(align_last_end = true) ~id ~cols ~rows sort =
  let thead = make_header id cols sort in
  let rows = make_rows rows in
  let classes =
    a_class (Component_table.table_classes layout ~align_last_end ())
  in
  table ~a:[ a_id id; classes ] ~thead rows
;;
