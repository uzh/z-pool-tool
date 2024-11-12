open Tyxml.Html
open Component.Input
open Pool_message
module Partials = Component.Partials
module Url = Page_admin_custom_fields.Url

let form
  ?custom_field_group
  current_model
  Pool_context.{ language; csrf; _ }
  tenant_languages
  flash_fetcher
  =
  let open Custom_field in
  let action =
    match custom_field_group with
    | None -> Url.Group.create_path current_model
    | Some (g, _) -> Url.Group.detail_path Group.(g.Group.model, g.id)
  in
  let input_by_lang ?required =
    Page_admin_custom_fields.input_by_lang
      ?required
      language
      tenant_languages
      flash_fetcher
      custom_field_group
  in
  let name_inputs =
    input_by_lang ~required:true Field.Name (fun lang (g, _) ->
      let open CCOption in
      g.Group.name
      |> Name.find_opt lang
      >|= Name.value_name
      |> value ~default:"")
  in
  let sort_fields_form =
    match custom_field_group with
    | None -> txt ""
    | Some (group, fields) ->
      let open Custom_field in
      div
        [ h2
            ~a:[ a_class [ "heading-2" ] ]
            [ txt
                (Field.CustomField
                 |> Pool_common.Utils.field_to_string language
                 |> CCString.capitalize_ascii)
            ]
        ; p
            Pool_common.
              [ Utils.hint_to_string
                  language
                  I18n.(CustomFieldSort Field.CustomFields)
                |> txt
              ]
        ; form
            ~a:
              [ a_class [ "stack" ]
              ; a_method `Post
              ; a_action
                  (Sihl.Web.externalize_path
                     (Url.Group.detail_path Group.(group.model, group.id)
                      |> Format.asprintf "%s/sort-fields"))
              ; a_user_data "detect-unsaved-changes" ""
              ]
            [ csrf_element csrf ()
            ; CCList.map
                (fun field ->
                  div
                    ~a:
                      [ a_class
                          [ "flexrow"
                          ; "flex-gap"
                          ; "justify-between"
                          ; "align-center"
                          ]
                      ]
                    [ div
                        [ txt (field |> name |> Name.find_opt_or language "-") ]
                    ; div
                        [ input
                            ~a:
                              [ a_input_type `Hidden
                              ; a_name Field.(CustomField |> array_key)
                              ; a_value (field |> id |> Id.value)
                              ]
                            ()
                        ]
                    ; Url.Field.edit_path (model field, id field)
                      |> edit_link ~classnames:[ "small" ]
                    ])
                fields
              |> Component.Sortable.create_sortable
            ; div
                ~a:[ a_class [ "flexrow" ] ]
                [ submit_element
                    ~classnames:[ "push" ]
                    language
                    Control.UpdateOrder
                    ~submit_type:`Primary
                    ()
                ]
            ]
        ]
  in
  div
    ~a:[ a_class [ "gap" ] ]
    [ form
        ~a:
          [ a_method `Post
          ; a_action (Sihl.Web.externalize_path action)
          ; a_class [ "stack-lg" ]
          ; a_user_data "detect-unsaved-changes" ""
          ]
        [ csrf_element csrf ()
        ; div
            ~a:[ a_class [ "stack" ] ]
            [ strong
                [ txt
                    Pool_common.(
                      Field.Name
                      |> Utils.field_to_string language
                      |> CCString.capitalize_ascii)
                ]
            ; div ~a:[ a_class [ "stack" ] ] name_inputs
            ]
        ; div
            ~a:[ a_class [ "flexrow" ] ]
            [ submit_element
                ~classnames:[ "push" ]
                language
                Control.(
                  let field = Some Field.CustomFieldGroup in
                  match custom_field_group with
                  | None -> Create field
                  | Some _ -> Update field)
                ~submit_type:`Primary
                ()
            ]
        ]
    ; sort_fields_form
    ]
;;

let detail
  ?custom_field_group
  current_model
  (Pool_context.{ language; _ } as context)
  sys_langauges
  flash_fetcher
  =
  let changelog_html =
    match custom_field_group with
    | None -> txt ""
    | Some ({ Custom_field.Group.id; _ }, _) ->
      let url =
        HttpUtils.Url.Admin.custom_field_groups_path
          current_model
          ~suffix:"changelog"
          ~id
          ()
        |> Uri.of_string
      in
      Component.Changelog.list context url None
  in
  div
    ~a:[ a_class [ "trim"; "safety-margin" ] ]
    [ div
        [ Partials.form_title language Field.CustomFieldGroup custom_field_group
        ; Page_admin_custom_fields.model_subtitle language current_model
        ]
    ; div
        ~a:[ a_class [ "stack-lg" ] ]
        [ form
            ?custom_field_group
            current_model
            context
            sys_langauges
            flash_fetcher
        ; changelog_html
        ]
    ]
;;
