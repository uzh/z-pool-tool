open Tyxml.Html
open Pool_common

let create ?active language (title, html) =
  let active =
    CCOption.map_or
      ~default:false
      (fun active -> Pool_common.I18n.equal_nav_link title active)
      active
    |> function
    | false -> []
    | true -> [ "active" ]
  in
  div
    ~a:[ a_class ("collapsible" :: active) ]
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

let list ?active language elements =
  div
    ~a:[ a_class [ "collapsible-list" ] ]
    (CCList.map (create ?active language) elements)
;;
