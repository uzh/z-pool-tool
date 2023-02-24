open Tyxml.Html
open Query

let retain_search_and_sort query =
  let open Pool_common.Message in
  let search =
    let open Search in
    query.search
    |> CCOption.map (fun { query; _ } -> [ Field.Query, query |> Query.value ])
  in
  let sort =
    let open Sort in
    query.sort
    |> CCOption.map (fun { column; order } ->
         [ Field.Order, column |> fst |> Field.show
         ; Field.SortOrder, order |> SortOrder.show
         ])
  in
  [ search; sort ] |> CCList.filter_map CCFun.id |> CCList.flatten
;;

(* TODO[timhub]: Limit number of displayed buttons *)
let pagination language query { Pagination.page; page_count; _ } =
  let page_list_classes = [ "btn"; "small" ] in
  let open Pagination in
  let add_page_param page =
    let open Pool_common.Message in
    add_field_query_params
      "?"
      ((Field.Page, CCInt.to_string page) :: retain_search_and_sort query)
  in
  let previous =
    let label =
      Pool_common.(Utils.control_to_string language Message.PreviousPage)
    in
    if Page.(value page > value default)
    then a ~a:[ a_href (add_page_param (Page.value page - 1)) ] [ txt label ]
    else span ~a:[] [ txt label ]
  in
  let next =
    let label =
      Pool_common.(Utils.control_to_string language Message.NextPage)
    in
    if PageCount.value page_count > Page.value page
    then a ~a:[ a_href (add_page_param (Page.value page + 1)) ] [ txt label ]
    else span ~a:[] [ txt label ]
  in
  let page_list =
    CCList.range 1 (PageCount.value page_count)
    |> CCList.map (fun i ->
         if CCInt.equal i (Page.value page)
         then
           span
             ~a:[ a_class ("primary" :: page_list_classes) ]
             [ txt (CCInt.to_string i) ]
         else
           a
             ~a:[ a_href (add_page_param i); a_class page_list_classes ]
             [ txt (CCInt.to_string i) ])
    |> div ~a:[ a_class [ "flexrow"; "flex-gap-xs" ] ]
  in
  div
    ~a:[ a_class [ "trim"; "measure" ] ]
    [ div
        ~a:
          [ a_class
              [ "flexrow"; "pagination"; "justify-between"; "align-center" ]
          ]
        [ previous; page_list; next ]
    ]
;;

let search language query =
  [ Component_input.input_element
      ?value:(query.search |> CCOption.map Search.query_string)
      language
      `Text
      Pool_common.Message.Field.Query
  ]
;;

let sort language sortable_fields query =
  let open Sort in
  let open Pool_common.Message in
  let field =
    let selected =
      query.sort |> CCOption.map (fun { column; _ } -> column |> fst)
    in
    Component_input.selector
      ~add_empty:true
      ~option_formatter:(Pool_common.Utils.field_to_string language)
      language
      Field.Order
      Field.show
      sortable_fields
      selected
      ()
  in
  let order =
    let selected = query.sort |> CCOption.map (fun { order; _ } -> order) in
    let open SortOrder in
    Component_input.selector
      ~option_formatter:(to_human language)
      language
      Field.SortOrder
      show
      all
      selected
      ()
  in
  [ field; order ]
;;

let search_and_sort language query sortable_fields =
  form
    ~a:[ a_method `Get; a_action "?"; a_class [ "flexrow"; "flex-gap" ] ]
    [ div ~a:[ a_class [ "grow" ] ] (search language query)
    ; div (sort language sortable_fields query)
    ]
;;

let create language to_table sortable_fields (items, query) =
  div
    ~a:[ a_class [ "stack" ] ]
    [ search_and_sort language query sortable_fields
    ; to_table items
    ; query.pagination
      |> CCOption.map_or ~default:(txt "") (pagination language query)
    ]
;;
