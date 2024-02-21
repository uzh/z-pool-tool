open Tyxml.Html

let make_tabs html links =
  let toggle_id = "tab-nav-toggle" in
  let nav =
    CCList.map
      (fun (label, url, active) ->
        match active, url with
        | true, _ | false, None ->
          span ~a:[ a_class [ "active" ] ] [ txt label ]
        | _, Some url ->
          a ~a:[ a_href (Sihl.Web.externalize_path url) ] [ txt label ])
      links
    |> fun links ->
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
      ; nav
          ~a:[ a_class [ "tab-nav"; "toggle-body"; "flexrow"; "flex-gap" ] ]
          links
      ]
  in
  div [ nav; div ~a:[ a_class [ "tab-body" ] ] html ]
;;
