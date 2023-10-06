open Tyxml.Html

let create msg =
  div
    [ div
        ~a:[ a_class [ "tooltip-wrapper" ] ]
        [ Component_icon.(to_html HelpOutline)
        ; p ~a:[ a_class [ "tooltip" ] ] [ txt msg ]
        ]
    ]
;;
