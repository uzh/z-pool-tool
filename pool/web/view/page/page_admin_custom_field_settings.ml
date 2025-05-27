open Tyxml.Html
open Component
open Pool_message

let show { Pool_context.csrf; language; _ } contact_fields =
  let open Pool_common in
  let open Custom_field in
  let action suffix =
    suffix
    |> Format.asprintf "/admin/custom-fields/settings/%s"
    |> Sihl.Web.externalize_path
  in
  let checkbox is_checked field =
    let checked = if is_checked field then [ a_checked () ] else [] in
    input
      ~a:
        ([ a_input_type `Checkbox
         ; a_name Field.(array_key CustomField)
         ; a_value (id field |> Id.value)
         ]
         @ checked)
      ()
  in
  let table getter =
    contact_fields
    |> CCList.map (fun field ->
      [ name_value language field |> txt; checkbox getter field ])
    |> Table.horizontal_table `Striped
  in
  let build_form setting_url title hint getter =
    div
      [ h2 ~a:[ a_class [ "has-gap" ] ] [ txt (Utils.text_to_string language title) ]
      ; p [ txt Utils.(text_to_string language hint) ]
      ; form
          ~a:
            [ a_method `Post
            ; a_action (action setting_url)
            ; a_class [ "flexcolumn"; "gap" ]
            ]
          [ Input.csrf_element csrf ()
          ; table getter
          ; div
              ~a:[ a_class [ "flexrow"; "gap" ] ]
              [ Input.submit_element
                  ~classnames:[ "push" ]
                  language
                  (Control.Save None)
                  ()
              ]
          ]
      ]
  in
  div
    ~a:[ a_class [ "trim"; "safety-margin"; "measure" ] ]
    [ h1 [ txt Utils.(nav_link_to_string language I18n.CustomFields) ]
    ; p [ txt Utils.(text_to_string language I18n.CustomFieldsSettings) ]
    ; div
        ~a:[ a_class [ "gap-lg"; "stack-lg" ] ]
        [ build_form
            "session-close"
            I18n.SessionCloseScreen
            I18n.CustomFieldsSettingsCloseScreen
            show_on_session_close_page
        ; build_form
            "session-detail"
            I18n.SessionDetailScreen
            I18n.CustomFieldsSettingsDetailScreen
            show_on_session_detail_page
        ]
    ]
;;
