open Tyxml.Html

let field_to_txt language =
  CCFun.(
    Pool_common.Utils.field_to_string language
    %> CCString.capitalize_ascii
    %> txt)
;;

let fields_to_txt language = CCList.map (field_to_txt language)

let table_head elements =
  elements |> CCList.map CCFun.(CCList.pure %> th) |> tr |> CCList.pure |> thead
;;

let layout_class = function
  | `Striped -> "striped"
  | `Simple -> "simple"
;;

let table_classes layout ?(align_top = false) ?(align_last_end = false) () =
  let base = [ "table"; layout_class layout ] in
  [ align_top, "align-top"; align_last_end, "align-last-end" ]
  |> CCList.filter_map (fun (add, str) -> if add then Some str else None)
  |> CCList.append base
;;

let legend_color_item classname =
  div
    ~a:
      [ a_class
          [ classname; "aspect-ratio"; "square"; "legend-item"; "legend-color" ]
      ]
    []
;;

let legend_icon_item icon =
  div ~a:[ a_class [ "legend-item" ] ] [ Component_icon.to_html icon ]
;;

let table_legend items =
  items
  |> CCList.map (fun (label, item) ->
    div
      ~a:[ a_class [ "flexrow"; "flex-gap"; "align-center" ] ]
      [ item; span [ label |> txt ] ])
  |> div ~a:[ a_class [ "flexcolumn"; "stack-sm" ] ]
;;

let horizontal_table
  layout
  ?(classnames = [])
  ?thead
  ?align_top
  ?align_last_end
  rows
  =
  let classes =
    table_classes layout ?align_top ?align_last_end () @ classnames
  in
  let thead = CCOption.map table_head thead in
  table
    ?thead
    ~a:[ a_class classes ]
    (CCList.map (fun row -> tr (CCList.map (fun cell -> td [ cell ]) row)) rows)
;;

let responsive_horizontal_table
  layout
  language
  header
  ?align_top
  ?align_last_end
  ?row_formatter
  rows
  =
  let classes = table_classes layout ?align_top ?align_last_end () in
  let header =
    header
    |> CCList.map
         (CCOption.map
            CCFun.(
              Pool_common.Utils.field_to_string language
              %> CCString.capitalize_ascii))
  in
  let thead =
    header
    |> CCList.map (fun h ->
      h |> CCOption.value ~default:"" |> txt |> CCList.pure |> th)
    |> tr
    |> CCList.pure
    |> thead
  in
  let find_label i = CCList.nth_opt header i |> CCOption.flatten in
  table
    ~thead
    ~a:[ a_class ("break-mobile" :: classes) ]
    (CCList.mapi
       (fun i row ->
         let cells =
           CCList.mapi
             (fun i cell ->
               match find_label i with
               | None -> td [ cell ]
               | Some label -> td ~a:[ a_user_data "label" label ] [ cell ])
             row
         in
         match row_formatter with
         | None -> tr cells
         | Some fnc ->
           i
           |> fnc
           |> (function
            | Some classnames -> tr ~a:[ a_class classnames ] cells
            | None -> tr cells))
       rows)
;;

let vertical_table layout language ?align_top ?(classnames = []) rows =
  let classes = table_classes layout ?align_top ~align_last_end:false () in
  table
    ~a:[ a_class (classes @ classnames) ]
    (CCList.map
       (fun (label, value) ->
         [ th
             [ txt
                 (Pool_common.Utils.field_to_string language label
                  |> CCString.capitalize_ascii)
             ]
         ; td [ value ]
         ]
         |> tr)
       rows)
;;
