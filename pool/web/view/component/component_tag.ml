open Tyxml.Html

let create ?remove_action language tag =
  let open Tags in
  let text = tag.title |> Title.value |> txt in
  let base_classes = [ "tag"; "primary" ] in
  let classnames, content =
    match remove_action with
    | None -> base_classes, [ text ]
    | Some (action, csrf) ->
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

let tag_list ?remove_action language tags =
  match tags with
  | [] ->
    p
      ~a:(if CCOption.is_some remove_action then [ a_class [ "help" ] ] else [])
      [ txt Pool_common.(Utils.text_to_string language I18n.SelectedTagsEmpty) ]
  | tags ->
    tags
    |> CCList.map (create ?remove_action language)
    |> div ~a:[ a_class [ "flexrow"; "wrap"; "flex-gap"; "align-start" ] ]
;;

let tag_form ?label language remove_action tags =
  let html = tag_list ~remove_action language tags in
  match label with
  | None -> html
  | Some i18n ->
    div
      ~a:[ a_class [ "form-group" ] ]
      [ Tyxml.Html.label
          [ txt Pool_common.(Utils.text_to_string language i18n) ]
      ; html
      ]
;;

let add_tags_form
  Pool_context.{ language; csrf; _ }
  ?(existing = [])
  available
  action
  =
  let available =
    CCList.(filter CCFun.(flip (mem ~eq:Tags.equal) existing %> not) available)
  in
  form
    ~a:[ a_method `Post; a_action action ]
    Component_input.
      [ csrf_element csrf ()
      ; div
          ~a:[ a_class [ "stack" ] ]
          [ selector
              ~add_empty:true
              ~option_formatter:Tags.(fun tag -> Title.value tag.title)
              language
              Pool_common.Message.Field.Tag
              Tags.(fun tag -> Id.value tag.id)
              available
              None
              ()
          ; div
              ~a:[ a_class [ "flexrow" ] ]
              [ submit_element
                  ~classnames:[ "push" ]
                  language
                  Pool_common.Message.(Add (Some Field.Tag))
                  ()
              ]
          ]
      ]
;;
