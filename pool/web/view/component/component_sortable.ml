open Tyxml.Html

let sortable_icon sortable =
  let classnames = [ "flexrow"; "flex-gap-sm" ] in
  div
    ~a:[ a_class classnames ]
    [ div
        ~a:[ a_class [ "flexrow"; "align-center" ] ]
        Component_icon.[ to_html Sort ]
    ; sortable
    ]
;;

let create ?(classnames = []) ?(attributes = []) children =
  div
    ~a:
      ([ a_user_data "sortable" ""; a_class ([ "grow" ] @ classnames) ]
       @ attributes)
    children
  |> sortable_icon
;;

let create_table ?(classnames = []) children =
  tablex
    ~a:[ a_class ([ "grow" ] @ classnames) ]
    [ tbody ~a:[ a_user_data "sortable" "" ] children ]
  |> sortable_icon
;;
