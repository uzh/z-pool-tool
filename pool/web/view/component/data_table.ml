open Tyxml.Html
module Icon = Component_icon

let hx_get ~url ~target =
  [ a_user_data "hx-get" url
  ; a_user_data "hx-push-url" url
  ; a_user_data "hx-target" target
  ; a_user_data "hx-swap" "innerHTML"
  ]
;;

type data_table =
  { url : Uri.t
  ; query : Query.t
  ; language : Pool_common.Language.t
  ; search : Query.Column.t list option
  ; additional_url_params : (Pool_common.Message.Field.t * string) list option
  }

let create_meta ?additional_url_params ?search url query language =
  { url; query; language; search; additional_url_params }
;;

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

let make_url ({ additional_url_params; url; query; _ } as sort) column =
  let query =
    query
    |> Query.with_sort_order (direction sort column)
    |> Query.with_sort_column column
    |> Query.to_uri_query ?additional_params:additional_url_params
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
    | Ascending -> Icon.CaretUp
    | Descending -> Icon.CaretDown
  in
  Component_icon.to_html icon
;;

let pagination
  { additional_url_params; language; url; query; _ }
  { Query.Pagination.page; page_count; _ }
  =
  let max_button_count = 7 in
  let button_count_threshold = 3 in
  let page_list_classes = [ "btn"; "small" ] in
  let open Query in
  let open Pool_common in
  let add_page_param page =
    let open Message in
    let page = [ Field.Page, CCInt.to_string page ] in
    let additional_params =
      additional_url_params
      |> CCOption.map_or ~default:page (CCList.append page)
    in
    Query.to_uri_query ~additional_params { query with pagination = None }
    |> Uri.with_query url
    |> Format.asprintf "%a" Uri.pp
    |> Sihl.Web.externalize_path
  in
  let open Pagination in
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

let searchbar
  ~target_id
  { additional_url_params; url; query; language; _ }
  searchable_by
  =
  let open Pool_common in
  let open Query in
  let search_field, search_label = Message.Field.(Search, Search |> show) in
  let url =
    Uri.with_query
      url
      (Query.to_uri_query
         ?additional_params:additional_url_params
         { query with search = None; pagination = None })
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

let make_head ?classname target_id sort column =
  let attrs =
    match classname with
    | None -> []
    | Some classname -> [ a_class [ classname ] ]
  in
  th
    ~a:attrs
    [ (match column with
       | `custom el -> el
       | `empty -> txt ""
       | `field (field, col) -> make_sortable_head target_id sort col field
       | `column col ->
         make_sortable_head target_id sort col (Query.Column.field col))
    ]
;;

let make_header ?th_class target_id cols sort =
  let classname i = CCOption.bind th_class (CCFun.flip CCList.nth_opt i) in
  thead
    [ tr
        (CCList.mapi
           (fun i col -> make_head ?classname:(classname i) target_id sort col)
           cols)
    ]
;;

let make
  ?(align_last_end = true)
  ?align_top
  ?(classnames = [])
  ?(layout = `Striped)
  ?(prepend_html = txt "")
  ?th_class
  ~target_id
  ~cols
  ~row
  data_table
  items
  =
  let default = txt "" in
  let search_bar =
    data_table.search
    |> CCOption.map_or ~default (searchbar ~target_id data_table)
  in
  let pagination =
    data_table.query.Query.pagination
    |> CCOption.map_or ~default (pagination data_table)
  in
  let thead = make_header ?th_class target_id cols data_table in
  let rows = CCList.map row items in
  let classes =
    a_class
      (Component_table.table_classes ?align_top layout ~align_last_end ()
       @ classnames)
  in
  div
    ~a:[ a_class [ "stack" ]; a_id target_id ]
    [ search_bar; prepend_html; table ~a:[ classes ] ~thead rows; pagination ]
;;
