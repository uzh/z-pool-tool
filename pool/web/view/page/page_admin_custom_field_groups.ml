open Tyxml.Html
open Component.Input
module Partials = Component.Partials
module Message = Pool_common.Message
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
    input_by_lang ~required:true Message.Field.Name (fun lang (g, _) ->
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
                (Message.Field.CustomField
                |> Pool_common.Utils.field_to_string language
                |> CCString.capitalize_ascii)
            ]
        ; form
            ~a:
              [ a_class [ "stack" ]
              ; a_method `Post
              ; a_action
                  (Sihl.Web.externalize_path
                     (Url.Group.detail_path Group.(group.model, group.id)
                     |> Format.asprintf "%s/sort-fields"))
              ]
            [ csrf_element csrf ()
            ; div
                ~a:[ a_user_data "sortable" "" ]
                (CCList.map
                   (fun field ->
                     div
                       ~a:
                         [ a_class
                             [ "flexrow"
                             ; "flex-gap"
                             ; "justify-between"
                             ; "align-center"
                             ]
                         ; a_user_data "sortable-item" ""
                         ]
                       [ div
                           [ txt (field |> name |> Name.find_opt_or language "-")
                           ]
                       ; div
                           [ input
                               ~a:
                                 [ a_input_type `Hidden
                                 ; a_name
                                     Message.Field.(CustomField |> array_key)
                                 ; a_value (field |> id |> Id.value)
                                 ]
                               ()
                           ]
                       ; div
                           ~a:
                             [ a_class [ "flexrow"; "flex-gap"; "align-center" ]
                             ]
                           [ a
                               ~a:
                                 [ a_href
                                     (Url.Field.edit_path (model field, id field)
                                     |> Sihl.Web.externalize_path)
                                 ]
                               [ txt
                                   Pool_common.(
                                     Message.More
                                     |> Utils.control_to_string language)
                               ]
                           ]
                       ])
                   fields)
            ; submit_element
                language
                Message.UpdateOrder
                ~submit_type:`Success
                ()
            ]
        ]
  in
  div
    ~a:[ a_class [ "stack-lg" ] ]
    [ form
        ~a:
          [ a_method `Post
          ; a_action (Sihl.Web.externalize_path action)
          ; a_class [ "stack-lg" ]
          ]
        [ csrf_element csrf ()
        ; div
            ~a:[ a_class [ "stack" ] ]
            [ h4
                ~a:[ a_class [ "heading-4" ] ]
                [ txt
                    Pool_common.(
                      Message.Field.Name
                      |> Utils.field_to_string language
                      |> CCString.capitalize_ascii)
                ]
            ; div ~a:[ a_class [ "stack" ] ] name_inputs
            ]
        ; submit_element
            language
            Message.(
              let field = Some Field.CustomField in
              match custom_field_group with
              | None -> Create field
              | Some _ -> Update field)
            ~submit_type:`Success
            ()
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
  div
    ~a:[ a_class [ "trim"; "safety-margin"; "measure" ] ]
    [ Partials.form_title
        language
        Message.Field.CustomFieldGroup
        custom_field_group
    ; Page_admin_custom_fields.model_subtitle language current_model
    ; form ?custom_field_group current_model context sys_langauges flash_fetcher
    ]
;;
