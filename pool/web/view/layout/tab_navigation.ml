open Tyxml.Html
open Entity
open Navigation_utils

let make_tabs ~actor ?overlay_title ?active_navigation context html nav_elements
  =
  let make_nav =
    create_nav ~actor ?active_navigation ~validate:true context nav_elements
  in
  let overlay_title =
    overlay_title
    |> CCOption.map (fun title ->
      div
        ~a:[ a_class [ "app-title"; "flex-grow"; "word-break-all" ] ]
        [ txt title ])
  in
  let nav =
    div
      ~a:[ a_class [ "tab-nav-container" ] ]
      [ div
          ~a:
            [ a_class
                [ "flexrow"; "flex-gap"; "justify-between"; "align-center" ]
            ]
          [ create_mobile_nav
              ?title:overlay_title
              ~toggle_id:"tab-navigation-overlay"
              make_nav
          ]
      ; div
          ~a:[ a_class [ "flexrow"; "flex-gap"; "hidden-mobile" ] ]
          (make_nav Horizonal)
      ]
  in
  [ nav; div ~a:[ a_class [ "tab-body" ] ] html ]
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

let with_heading title children =
  div
    ~a:[ a_class [ "trim"; "safety-margin" ] ]
    (h1 ~a:[ a_class [ "heading-1" ] ] [ txt title ] :: children)
;;
