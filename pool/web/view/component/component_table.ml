open Tyxml.Html

let table_head language fields =
  CCList.map
    (fun field ->
      th
        [ txt
            (CCOption.map_or
               ~default:""
               (fun f ->
                 Pool_common.Utils.field_to_string language f
                 |> CCString.capitalize_ascii)
               field)
        ])
    fields
  |> tr
  |> CCList.pure
  |> thead
;;

let layout_class = function
  | `Striped -> "striped"
  | `Simple -> "simple"
;;

let table_classes layout align_top =
  let base = [ "table"; layout_class layout ] in
  match align_top with
  | true -> CCList.cons "align-top" base
  | false -> base
;;

let horizontal_table layout language ?thead ?(align_top = false) rows =
  let classes = table_classes layout align_top in
  let thead = CCOption.map (fun thead -> thead |> table_head language) thead in
  table
    ?thead
    ~a:[ a_class classes ]
    (CCList.map (fun row -> tr (CCList.map (fun cell -> td [ cell ]) row)) rows)
;;

let vertical_table layout language ?(align_top = false) rows =
  let classes = table_classes layout align_top in
  table
    ~a:[ a_class classes ]
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
