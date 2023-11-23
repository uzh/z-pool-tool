open Tyxml.Html

let chevron_icon = Component_icon.(to_html ChevronForward)
let info_icon = Component_icon.(to_html HelpOutline)

let create title html =
  div
    ~a:[ a_class [ "collapsible"; "note"; "standalone" ] ]
    [ div
        ~a:
          [ a_class [ "collapsible-header" ]
          ; a_aria "role" [ "button" ]
          ; a_aria "expanded" [ "false" ]
          ]
        [ span ~a:[ a_class [ "collapsible-title" ] ] [ chevron_icon; title ] ]
    ; div
        ~a:[ a_class [ "collapsible-body" ]; a_aria "role" [ "region" ] ]
        [ div ~a:[ a_class [ "collapsible-content" ] ] [ html ] ]
    ]
;;

let create_note ?(icon = Some info_icon) ?title language =
  let title =
    title
    |> CCOption.value
         ~default:Pool_common.(Utils.text_to_string language I18n.Note)
    |> fun title ->
    span
      ~a:[ a_class [ "has-icon"; "flex-gap-xs" ] ]
      (txt title :: CCOption.map_or ~default:[] CCList.return icon)
  in
  create title
;;
