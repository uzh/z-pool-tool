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

let horizontal_table layout language ?thead rows =
  let thead = CCOption.map (fun thead -> thead |> table_head language) thead in
  table
    ?thead
    ~a:[ a_class [ "table"; layout_class layout ] ]
    (CCList.map (fun row -> tr (CCList.map (fun cell -> td [ cell ]) row)) rows)
;;

let vertical_table layout language rows =
  table
    ~a:[ a_class [ "table"; layout_class layout ] ]
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
