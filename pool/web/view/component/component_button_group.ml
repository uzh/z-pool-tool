open Tyxml.Html

let dropdown
  ?(classnames = [])
  ?(icon = Component_icon.EllipsisVertical)
  ?(icon_style = [ "small"; "is-text" ])
  ?(orientation = `Right)
  buttons
  =
  let orientation_cls = function
    | `Left -> "left"
    | `Right -> "right"
  in
  let open Component_icon in
  div
    ~a:[ a_class ("button-list" :: classnames) ]
    [ div
        ~a:[ a_class [ "has-dropdown"; orientation_cls orientation ] ]
        [ button ~a:[ a_class ("btn" :: icon_style) ] [ to_html icon ]
        ; ul
            ~a:[ a_class [ "dropdown" ] ]
            (buttons |> CCList.map CCFun.(CCList.return %> li))
        ]
    ]
;;
