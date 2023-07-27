open Tyxml.Html
open Pool_common

let create language (title, html) =
  div
    ~a:[ a_class [ "collapsible" ] ]
    [ div
        ~a:[ a_class [ "collapsible-header" ] ]
        [ span
            ~a:[ a_class [ "collapsible-title" ] ]
            [ Component_icon.(to_html Close)
            ; txt (Utils.nav_link_to_string language title)
            ]
        ]
    ; div ~a:[ a_class [ "collapsible-body" ] ] html
    ]
;;

let list language elements =
  div
    ~a:[ a_class [ "collapsible-list" ] ]
    (CCList.map (create language) elements)
;;
