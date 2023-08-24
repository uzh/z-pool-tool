open Tyxml.Html

let dropdown buttons =
  div
    ~a:[ a_class [ "button-list" ] ]
    [ div [ Component_icon.(to_html EllipsisVertical) ]
    ; ul
        ~a:[ a_class [ "dropdown" ] ]
        (buttons |> CCList.map CCFun.(CCList.return %> li))
    ]
;;
