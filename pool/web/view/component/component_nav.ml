open Tyxml.Html

let make_tabs html navigation =
  let toggle_id = "tab-nav-toggle" in
  let nav =
    div
      ~a:[ a_class [ "tab-nav-container" ] ]
      [ div
          ~a:[ a_class [ "tab-nav-header" ] ]
          [ label
              ~a:[ a_class [ "icon-lg" ]; a_label_for toggle_id ]
              Component_icon.[ to_html MenuOutline ]
          ]
      ; input
          ~a:[ a_input_type `Checkbox; a_class [ "toggle" ]; a_id toggle_id ]
          ()
      ; div ~a:[ a_class [ "flexrow"; "flex-gap" ] ] navigation
      ]
  in
  div [ nav; div ~a:[ a_class [ "tab-body" ] ] html ]
;;

let make_body ?buttons ?hint language title children =
  let title =
    let base = h2 ~a:[ a_class [ "heading-2" ] ] [ txt title ] in
    let title =
      let classnames =
        [ "flexrow"; "justify-between"; "flex-gap"; "flexcolumn-mobile" ]
      in
      CCOption.map_or
        ~default:base
        (fun buttons ->
          div ~a:[ a_class classnames ] [ div [ base ]; div [ buttons ] ])
        buttons
    in
    CCOption.map_or
      ~default:[ title ]
      (fun hint ->
        [ title
        ; p
            [ Pool_common.Utils.hint_to_string language hint
              |> Http_utils.add_line_breaks
            ]
        ])
      hint
  in
  title @ [ div ~a:[ a_class [ "gap-lg" ] ] children ]
;;
