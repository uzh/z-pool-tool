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

let table_classes layout align_top align_last_end =
  let base = [ "table"; layout_class layout ] in
  [ align_top, "align-top"; align_last_end, "align-last-end" ]
  |> CCList.filter_map (fun (add, str) -> if add then Some str else None)
  |> CCList.append base
;;

let horizontal_table
  layout
  ?(classnames = [])
  ?thead
  ?(align_top = false)
  ?(align_last_end = false)
  rows
  =
  let classes = table_classes layout align_top align_last_end @ classnames in
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
  ?(align_top = false)
  ?(align_last_end = false)
  rows
  =
  let classes = table_classes layout align_top align_last_end in
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
    (CCList.map
       (fun row ->
         tr
           (CCList.mapi
              (fun i cell ->
                match find_label i with
                | None -> td [ cell ]
                | Some label -> td ~a:[ a_user_data "label" label ] [ cell ])
              row))
       rows)
;;

let vertical_table layout language ?(align_top = false) ?(classnames = []) rows =
  let classes = table_classes layout align_top false in
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
