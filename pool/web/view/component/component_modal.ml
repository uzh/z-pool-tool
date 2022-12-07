open Tyxml.Html

let create ?subtitle language title id html =
  let title =
    div
      ~a:[ a_class [ "modal-heading" ] ]
      [ txt (title language)
      ; span ~a:[ a_class [ "modal-close" ] ] [ Component_icon.icon `Close ]
      ]
  in
  let subtitle =
    subtitle
    |> CCOption.map_or ~default:(txt "") (fun sub ->
         sub language |> txt |> CCList.pure |> p ~a:[ a_class [ "gap-xs" ] ])
  in
  div
    ~a:
      [ a_id id
      ; a_class [ "fullscreen-overlay"; "modal" ]
      ; a_aria "hidden" [ "true" ]
      ]
    [ div
        ~a:[ a_class [ "modal-body" ] ]
        [ div ~a:[ a_class [ "modal-header" ] ] [ title; subtitle ]
        ; div ~a:[ a_class [ "modal-content" ] ] [ html ]
        ]
    ]
;;
