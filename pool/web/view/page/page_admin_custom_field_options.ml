open Tyxml.Html
open Component
module Message = Pool_common.Message

let base_path field =
  let open Custom_field in
  let base = Page_admin_custom_fields.base_field_path (model field) in
  Format.asprintf "%s/%s/options" base (field |> id |> Id.value)
;;

let option_form
  ?(custom_field_option : Custom_field.SelectOption.t option)
  custom_field
  Pool_context.{ language; csrf; _ }
  tenant_languages
  flash_fetcher
  =
  let open Custom_field in
  let action =
    let base = base_path custom_field in
    match custom_field_option with
    | None -> base
    | Some o -> Format.asprintf "%s/%s" base SelectOption.(o.id |> Id.value)
  in
  let name_inputs =
    Page_admin_custom_fields.input_by_lang
      ~required:true
      language
      tenant_languages
      flash_fetcher
      custom_field_option
      Message.Field.Name
      (fun lang o ->
      let open CCOption in
      o.SelectOption.name
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
    ; div ~a:[ a_class [ "stack" ] ] name_inputs
    ; submit_element
        language
        Message.(
          let field = Some Field.CustomField in
          match custom_field_option with
          | None -> Create field
          | Some _ -> Update field)
        ~submit_type:`Success
        ()
    ]
;;

let detail
  ?custom_field_option
  custom_field
  (Pool_context.{ language; csrf; _ } as context)
  sys_languages
  flash_fetcher
  =
  let title =
    Pool_common.(
      Utils.control_to_string
        language
        Message.(
          if CCOption.is_none custom_field_option
          then Create (Some Field.CustomFieldOption)
          else Update (Some Field.CustomFieldOption)))
  in
  let delete_form =
    match custom_field_option with
    | None -> txt ""
    | Some option ->
      form
        ~a:
          [ a_method `Post
          ; a_action
              (Page_admin_custom_fields.make_option_url
                 custom_field
                 option
                 Custom_field.(model custom_field)
                 "delete")
          ; a_user_data
              "confirmable"
              Pool_common.(
                Utils.confirmable_to_string
                  language
                  I18n.DeleteCustomFieldOption)
          ]
        [ Component.csrf_element csrf ()
        ; submit_element ~submit_type:`Error language Message.(Delete None) ()
        ]
  in
  div
    ~a:[ a_class [ "trim"; "safety-margin"; "measure" ] ]
    [ h1 [ txt title ]
    ; div
        ~a:[ a_class [ "stack" ] ]
        [ option_form
            ?custom_field_option
            custom_field
            context
            sys_languages
            flash_fetcher
        ; delete_form
        ; p
            [ a
                ~a:
                  [ a_href
                      (Format.asprintf
                         "/admin/custom-fields/%s/edit"
                         Custom_field.(id custom_field |> Id.value)
                      |> Sihl.Web.externalize_path)
                  ]
                [ txt
                    Pool_common.(
                      Utils.control_to_string language Message.(Back))
                ]
            ]
        ]
    ]
;;
