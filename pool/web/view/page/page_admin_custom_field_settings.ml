open Tyxml.Html
module Icon = Component.Icon
module Table = Component.Table
module Partials = Component.Partials
module Message = Pool_common.Message

let show { Pool_context.language; _ } contact_fields =
  let open Pool_common in
  let open Custom_field in
  let action suffix =
    Format.asprintf "/admin/custom-fields/settings/%s" suffix
    |> Sihl.Web.externalize_path
  in
  let checkbox field =
    input
      ~a:
        [ a_input_type `Checkbox
        ; a_name Message.Field.(array_key CustomField)
        ; a_value (id field |> Id.value)
        ]
      ()
  in
  let table =
    contact_fields
    |> CCList.map (fun field ->
      [ name_value language field |> txt; checkbox field ])
    |> Component.Table.horizontal_table `Striped
  in
  div
    ~a:[ a_class [ "trim"; "safety-margin"; "measure" ] ]
    [ h1 [ txt Utils.(nav_link_to_string language I18n.CustomFields) ]
    ; p [ txt Utils.(text_to_string language I18n.CustomFieldsSettings) ]
    ; form
        ~a:
          [ a_method `Post
          ; a_action (action "contact")
          ; a_class [ "flexcolumn" ]
          ]
        [ table
        ; div
            ~a:[ a_class [ "flexrow"; "gap" ] ]
            [ Component.Input.submit_element
                ~classnames:[ "push" ]
                language
                (Message.Save None)
                ()
            ]
        ]
    ]
;;
