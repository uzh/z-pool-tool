open Tyxml.Html
open Component

let show { Pool_context.csrf; language; _ } contact_fields =
  let open Pool_common in
  let open Custom_field in
  let action = "/admin/custom-fields/settings" |> Sihl.Web.externalize_path in
  let checkbox field =
    let checked =
      if show_on_session_close_page field then [ a_checked () ] else []
    in
    input
      ~a:
        ([ a_input_type `Checkbox
         ; a_name Message.Field.(array_key CustomField)
         ; a_value (id field |> Id.value)
         ]
         @ checked)
      ()
  in
  let table =
    contact_fields
    |> CCList.map (fun field ->
      [ name_value language field |> txt; checkbox field ])
    |> Table.horizontal_table `Striped
  in
  div
    ~a:[ a_class [ "trim"; "safety-margin"; "measure" ] ]
    [ h1 [ txt Utils.(nav_link_to_string language I18n.CustomFields) ]
    ; p [ txt Utils.(text_to_string language I18n.CustomFieldsSettings) ]
    ; form
        ~a:[ a_method `Post; a_action action; a_class [ "flexcolumn" ] ]
        [ Input.csrf_element csrf ()
        ; table
        ; div
            ~a:[ a_class [ "flexrow"; "gap" ] ]
            [ Input.submit_element
                ~classnames:[ "push" ]
                language
                (Message.Save None)
                ()
            ]
        ]
    ]
;;
