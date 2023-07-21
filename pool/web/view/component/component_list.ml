open Tyxml.Html
open Query
module Icon = Component_icon

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

let search language query searchable_by =
  [ Component_input.input_element
      ?value:(query.search |> CCOption.map Search.query_string)
      ~help:
        (Pool_common.I18n.SearchByFields (CCList.map Column.field searchable_by))
      language
      `Text
      Pool_common.Message.Field.Search
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

let search_and_sort language query sortable_by searchable_by =
  form
    ~a:[ a_method `Get; a_action "?"; a_class [ "flexcolumn"; "flex-gap" ] ]
    [ div
        ~a:[ a_class [ "flexrow"; "flex-gap"; "flexcolumn-mobile" ] ]
        [ div ~a:[ a_class [ "grow-3" ] ] (search language query searchable_by)
        ; div
            ~a:[ a_class [ "flexrow"; "flex-gap"; "grow-1" ] ]
            (sort language sortable_by query)
        ]
    ; div
        ~a:[ a_class [ "flexrow" ] ]
        [ div
            ~a:[ a_class [ "push"; "flexrow"; "flex-gap"; "align-center" ] ]
            [ a
                ~a:[ a_href "?" ]
                [ txt
                    Pool_common.(Utils.control_to_string language Message.Reset)
                ]
            ; Component_input.submit_element
                ~classnames:[ "small" ]
                language
                Pool_common.Message.Apply
                ()
            ]
        ]
    ]
;;

let create language to_table sortable_by searchable_by (items, query) =
  div
    ~a:[ a_class [ "stack" ] ]
    [ search_and_sort language query sortable_by searchable_by
    ; to_table items
    ; query.pagination
      |> CCOption.map_or ~default:(txt "") (pagination language query)
    ]
;;
