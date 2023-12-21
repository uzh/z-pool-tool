open Tyxml.Html
open Query
module Icon = Component_icon

let retain_search_and_sort query =
  let open Pool_common.Message in
  let search =
    let open Search in
    query.search
    |> CCOption.map (fun { query; _ } -> [ Field.Search, query |> Query.value ])
  in
  let sort =
    let open Sort in
    query.sort
    |> CCOption.map (fun { column; order } ->
      [ Field.Order, column |> Column.field |> Field.show
      ; Field.SortOrder, order |> SortOrder.show
      ])
  in
  [ search; sort ] |> CCList.filter_map CCFun.id |> CCList.flatten
;;

let pagination language query { Pagination.page; page_count; _ } =
  let max_button_count = 7 in
  let button_count_threshold = 3 in
  let page_list_classes = [ "btn"; "small" ] in
  let open Pagination in
  let open Pool_common in
  let add_page_param page =
    let open Message in
    add_field_query_params
      "?"
      ((Field.Page, CCInt.to_string page) :: retain_search_and_sort query)
  in
  let previous =
    let label = Utils.control_to_string language Message.PreviousPage in
    let icon = Icon.(to_html ~classnames:[ "icon-lg" ] PrevCircleOutline) in
    if Page.(value page > value default)
    then
      a
        ~a:
          [ a_href (add_page_param (Page.value page - 1))
          ; a_class [ "has-icon"; "undecorated" ]
          ; a_aria "label" [ label ]
          ]
        [ icon; span ~a:[ a_class [ "hidden-mobile" ] ] [ txt label ] ]
    else span ~a:[ a_class [ "has-icon" ] ] [ icon ]
  in
  let next =
    let label = Utils.control_to_string language Message.NextPage in
    let icon = Icon.(to_html ~classnames:[ "icon-lg" ] NextCircleOutline) in
    if PageCount.value page_count > Page.value page
    then
      a
        ~a:
          [ a_href (add_page_param (Page.value page + 1))
          ; a_class [ "has-icon"; "undecorated" ]
          ; a_aria "label" [ label ]
          ]
        [ span ~a:[ a_class [ "hidden-mobile" ] ] [ txt label ]; icon ]
    else span ~a:[ a_class [ "has-icon" ] ] [ icon ]
  in
  let page_list =
    let create buttons =
      buttons
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
    in
    let wrap = div ~a:[ a_class [ "flexrow"; "flex-gap-xs" ] ] in
    let create_grouped (buttons : int list list) =
      let spacer = span ~a:[ a_class [ "inset-xs" ] ] [ txt "..." ] in
      let rec build html buttons =
        match buttons with
        | [] -> html
        | [ tl ] -> html @ (tl |> create)
        | hd :: tl -> build (html @ (hd |> create) @ [ spacer ]) tl
      in
      build [] buttons |> wrap
    in
    let page_count = PageCount.value page_count in
    let current = Page.value page in
    let range = CCList.range in
    match page_count with
    | _ when page_count > max_button_count ->
      (match current with
       | _ when current <= button_count_threshold ->
         let left = range 1 (button_count_threshold + 1) in
         let right = [ page_count ] in
         [ left; right ] |> create_grouped
       | _ when current >= page_count - button_count_threshold ->
         let left = [ 1 ] in
         let right =
           range (page_count - button_count_threshold - 1) page_count
         in
         [ left; right ] |> create_grouped
       | _ ->
         let left = [ 1 ] in
         let mid = range (current - 1) (current + 1) in
         let right = [ page_count ] in
         [ left; mid; right ] |> create_grouped)
    | _ -> range 1 page_count |> create |> wrap
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

let hx_get ~url ~target =
  [ a_user_data "hx-get" url
  ; a_user_data "hx-push-url" "true"
  ; a_user_data "hx-target" target
  ; a_user_data "hx-swap" "innerHTML"
  ]
;;

let searchbar ~url ~target_id language query searchable_by =
  let open Pool_common in
  let search_field, search_label = Message.Field.(Search, Search |> show) in
  let url =
    Uri.with_query url (Query.to_uri_query { query with search = None })
    |> Format.asprintf "%a" Uri.pp
    |> Sihl.Web.externalize_path
  in
  div
    ~a:[ a_class [ "form-group" ] ]
    [ label
        ~a:[ a_label_for search_label ]
        [ search_field |> Utils.field_to_string_capitalized language |> txt ]
    ; div
        ~a:[ a_class [ "flexrow"; "flex-gap"; "flexcolumn-mobile" ] ]
        [ div
            ~a:[ a_class [ "flexcolumn"; "grow" ] ]
            [ input
                ~a:
                  ([ a_name search_label
                   ; a_id search_label
                   ; a_input_type `Search
                   ; a_value
                       (query.search
                        |> CCOption.map_or ~default:"" Search.query_string)
                   ; a_user_data
                       "hx-trigger"
                       "input changed delay:300ms, search"
                   ]
                   @ hx_get ~url ~target:("#" ^ target_id))
                ()
            ; span
                ~a:[ a_class [ "help" ] ]
                [ I18n.SearchByFields (CCList.map Column.field searchable_by)
                  |> Utils.hint_to_string language
                  |> txt
                ]
            ]
        ; a
            ~a:[ a_class [ "btn"; "small"; "is-text"; "gap-sm" ]; a_href "?" ]
            [ txt (Utils.control_to_string language Message.(Reset None)) ]
        ]
    ]
;;

let sort language sortable_by query =
  let open Sort in
  let open Pool_common.Message in
  let classnames = [ "grow" ] in
  let field =
    let selected =
      query.sort |> CCOption.map (fun { column; _ } -> column |> Column.field)
    in
    Component_input.selector
      ~add_empty:true
      ~option_formatter:(Pool_common.Utils.field_to_string language)
      ~classnames
      language
      Field.Order
      Field.show
      (CCList.map Column.field sortable_by)
      selected
      ()
  in
  let order =
    let selected = query.sort |> CCOption.map (fun { order; _ } -> order) in
    let open SortOrder in
    Component_input.selector
      ~option_formatter:(to_human language)
      ~classnames
      language
      Field.SortOrder
      show
      all
      selected
      ()
  in
  [ field; order ]
;;

let create ?legend ~url ~target_id language to_table searchable_by (items, query)
  =
  (if CCList.is_empty searchable_by
   then []
   else [ searchbar ~url ~target_id language query searchable_by ])
  @ [ CCOption.value ~default:(txt "") legend
    ; to_table items
    ; query.pagination
      |> CCOption.map_or ~default:(txt "") (pagination language query)
    ]
  |> div ~a:[ a_id target_id; a_class [ "stack"; "hx-search" ] ]
;;
