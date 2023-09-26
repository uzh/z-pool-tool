open Tyxml.Html

let create ?(active = false) ?subtitle language title id html =
  let title =
    div
      ~a:[ a_class [ "modal-heading" ] ]
      [ txt (title language)
      ; span ~a:[ a_class [ "modal-close" ] ] Component_icon.[ to_html Close ]
      ]
  in
  let subtitle =
    subtitle
    |> CCOption.map_or ~default:(txt "") (fun sub ->
      sub language |> txt |> CCList.pure |> p ~a:[ a_class [ "gap-xs" ] ])
  in
  let attrs =
    let base = [ "fullscreen-overlay"; "modal" ] in
    match active with
    | true -> [ a_class ("active" :: base); a_aria "hidden" [ "false" ] ]
    | false -> [ a_class base; a_aria "hidden" [ "true" ] ]
  in
  div
    ~a:(a_id id :: attrs)
    [ div
        ~a:[ a_class [ "modal-body" ] ]
        [ div ~a:[ a_class [ "modal-header" ] ] [ title; subtitle ]
        ; div ~a:[ a_class [ "modal-content" ] ] [ html ]
        ]
    ]
;;
