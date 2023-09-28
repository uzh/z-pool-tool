open Tyxml.Html

let dropdown ?(classnames = []) ?(orientation = `Right) buttons =
  let orientation_cls = function
    | `Left -> "left"
    | `Right -> "right"
  in
  div
    ~a:[ a_class ("button-list" :: classnames) ]
    [ div
        ~a:[ a_class [ "has-dropdown"; orientation_cls orientation ] ]
        [ button
            ~a:[ a_class [ "btn"; "small"; "is-text" ] ]
            [ Component_icon.(to_html EllipsisVertical) ]
        ; ul
            ~a:[ a_class [ "dropdown" ] ]
            (buttons |> CCList.map CCFun.(CCList.return %> li))
        ]
    ]
;;
