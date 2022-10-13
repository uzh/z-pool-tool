open Tyxml.Html
open Component
module Message = Pool_common.Message
module Url = Page_admin_custom_fields.Url

let form
  ?(custom_field_group : Custom_field.Group.t option)
  current_model
  Pool_context.{ language; csrf; _ }
  tenant_languages
  flash_fetcher
  =
  let open Custom_field in
  let action =
    match custom_field_group with
    | None -> Url.Group.create_path current_model
    | Some g -> Url.Group.detail_path Group.(g.model, g.id)
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
    input_by_lang ~required:true Message.Field.Name (fun lang g ->
      let open CCOption in
      g.Group.name
      |> Name.find_opt lang
      >|= Name.value_name
      |> value ~default:"")
  in
  form
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
;;

let detail
  ?custom_field_group
  current_model
  (Pool_context.{ language; _ } as context)
  sys_langauges
  flash_fetcher
  =
  let title =
    Pool_common.(
      Utils.control_to_string
        language
        Message.(
          if CCOption.is_none custom_field_group
          then Create (Some Field.CustomFieldGroup)
          else Update (Some Field.CustomFieldGroup)))
  in
  div
    ~a:[ a_class [ "trim"; "safety-margin"; "measure" ] ]
    [ h1 ~a:[ a_class [ "heading-1" ] ] [ txt title ]
    ; Page_admin_custom_fields.model_subtitle language current_model
    ; form ?custom_field_group current_model context sys_langauges flash_fetcher
    ]
;;
