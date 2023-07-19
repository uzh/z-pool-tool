open Tyxml.Html

let create ?remove_action tag =
  let open Tags in
  let text = tag.title |> Title.value |> txt in
  let base_classes = [ "tag"; "primary" ] in
  let classnames, content =
    match remove_action with
    | None -> base_classes, [ text ]
    | Some (action, csrf, language) ->
      ( "has-icon" :: base_classes
      , [ text
        ; form
            ~a:
              [ a_method `Post
              ; a_action (action tag)
              ; a_user_data
                  "confirmable"
                  Pool_common.(
                    Utils.confirmable_to_string language I18n.RemoveTag)
              ]
            [ Component_input.csrf_element csrf ()
            ; button
                ~a:[ a_button_type `Submit ]
                [ Component_icon.(to_html Close) ]
            ]
        ] )
  in
  div ~a:[ a_class classnames ] content
;;

let tag_list ?remove_action tags =
  tags
  |> CCList.map (create ?remove_action)
  |> div ~a:[ a_class [ "flexrow"; "wrap"; "flex-gap"; "align-start" ] ]
;;
