open Tyxml.Html
open Query

(* TODO[timhub]: Limit number of displayed buttons *)
let pagination language { Pagination.page; page_count; _ } =
  Logs.info (fun m -> m "page: %i" (page |> Pagination.Page.value));
  Logs.info (fun m ->
    m "page count: %i" (page_count |> Pagination.PageCount.value));
  let page_list_classes = [ "btn"; "small" ] in
  let open Pagination in
  let add_page_param page =
    Pool_common.Message.(
      add_field_query_params "?" [ Field.Page, CCInt.to_string page ])
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

let search language =
  form
    ~a:[ a_method `Get; a_action "?" ]
    [ Component_input.input_element
        language
        `Text
        Pool_common.Message.Field.Query
    ]
;;

let create language to_table (items, query) =
  div
    ~a:[ a_class [ "stack" ] ]
    [ search language
    ; to_table items
    ; query.pagination
      |> CCOption.map_or ~default:(txt "") (pagination language)
    ]
;;
