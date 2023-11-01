open Tyxml.Html

let icon = Component_icon.(to_html ReorderTwo)
let sortable_list_attrib = a_user_data "sortable" ""
let sortable_item_attrib = a_user_data "sortable-item" ""

let sortable_item sortable =
  div
    ~a:
      [ a_class [ "flexrow"; "flex-gap-sm"; "inset-sm"; "align-center" ]
      ; sortable_item_attrib
      ]
    [ div [ icon ]; sortable ]
;;

let create_sortable ?(classnames = []) ?(attributes = []) sortables =
  sortables
  |> CCList.map sortable_item
  |> div
       ~a:
         ([ sortable_list_attrib; a_class ([ "grow" ] @ classnames) ]
          @ attributes)
;;

let create_table ?(classnames = []) sortable_cells =
  let open CCList in
  let icon = td [ icon ] in
  sortable_cells
  >|= (fun cells -> icon :: cells |> tr ~a:[ sortable_item_attrib ])
  |> tbody ~a:[ sortable_list_attrib ]
  |> return
  |> tablex ~a:[ a_class ("sortable" :: classnames) ]
;;
