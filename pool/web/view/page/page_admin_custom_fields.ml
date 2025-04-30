open Tyxml.Html
open Component.Input
open Pool_message
module Icon = Component.Icon
module Table = Component.Table
module Partials = Component.Partials

module Url = struct
  open Custom_field

  let concat = CCString.concat "/"
  let fallback_path = "/admin/custom-fields"
  let index_path m = [ fallback_path; Model.show m ] |> concat

  module Field = struct
    let field_key = "field"
    let create_path m = [ index_path m; field_key ] |> concat
    let new_path m = [ index_path m; field_key; "new" ] |> concat
    let detail_path (m, id) = [ index_path m; field_key; id |> Id.value ] |> concat
    let edit_path (m, id) = [ detail_path (m, id); "edit" ] |> concat
  end

  module Option = struct
    open SelectOption

    let options_key = "options"
    let index_path = Field.detail_path
    let new_path field = [ index_path field; options_key; "new" ] |> concat
    let create_path field = [ Field.detail_path field; options_key ] |> concat
    let detail_path field id = [ index_path field; options_key; id |> Id.value ] |> concat
    let edit_path field id = [ detail_path field id; "edit" ] |> concat
  end

  module Group = struct
    open Group

    let group_key = "group"
    let index_path = index_path
    let new_path m = [ index_path m; group_key; "new" ] |> concat
    let create_path m = [ index_path m; group_key ] |> concat

    let detail_path (model, id) =
      [ index_path model; group_key; id |> Id.value ] |> concat
    ;;

    let edit_path group = [ detail_path group; "edit" ] |> concat
    let delete_path group = [ detail_path group; "delete" ] |> concat
  end
end

let model_subtitle language model =
  div
    [ strong
        [ txt
            (Format.asprintf
               "%s: "
               (Pool_common.Utils.field_to_string language Field.Model
                |> CCString.capitalize_ascii))
        ]
    ; txt (Custom_field.Model.show model |> CCString.capitalize_ascii)
    ]
;;

let input_by_lang
      ?(required = false)
      ?flash_fetcher
      language
      tenant_languages
      elm
      field
      value_fnc
  =
  let open Pool_common in
  let group_class = Elements.group_class [] `Horizontal in
  CCList.map
    (fun lang ->
       let required = required && CCList.mem ~eq:Language.equal lang tenant_languages in
       let label_text =
         lang
         |> Language.field_of_t
         |> Utils.field_to_string language
         |> CCString.capitalize_ascii
       in
       let id = Format.asprintf "%s-%s" (Field.show field) (Language.show lang) in
       let value =
         let open CCOption in
         bind flash_fetcher (fun f ->
           f (Format.asprintf "%s[%s]" (Field.show field) (Language.show lang)))
         <+> (elm >|= value_fnc lang)
         |> value ~default:""
       in
       let input_element =
         let attrs =
           [ a_input_type `Text
           ; a_id id
           ; a_name (Format.asprintf "%s[%s]" (Field.show field) (Language.show lang))
           ; a_value value
           ]
         in
         div
           ~a:[ a_class [ "input-group" ] ]
           [ input ~a:(if required then a_required () :: attrs else attrs) () ]
       in
       div
         ~a:[ a_class group_class ]
         [ label
             ~a:[ a_label_for id ]
             [ txt (if required then Format.asprintf "%s *" label_text else label_text) ]
         ; input_element
         ])
    Language.all
;;

let field_form
      ?(custom_field : Custom_field.t option)
      ?flash_fetcher
      current_model
      Pool_context.{ language; csrf; _ }
      groups
      tenant_languages
  =
  let open CCFun in
  let open Custom_field in
  let open Pool_common in
  let action =
    match custom_field with
    | None -> Url.Field.create_path current_model
    | Some f -> Url.Field.detail_path (model f, id f)
  in
  let checkbox_element
        ?(disabled = false)
        ?orientation
        ?hints
        ?(default = false)
        ?append_html
        field
        fnc
    =
    checkbox_element
      language
      ~additional_attributes:(if disabled then [ a_disabled () ] else [])
      ?append_html
      ?flash_fetcher
      ?orientation
      ?hints
      field
      ~value:(custom_field |> CCOption.map_or ~default fnc)
  in
  let warning hint =
    div
      [ div
          ~a:[ a_class [ "help"; "error-message" ] ]
          [ txt (Utils.hint_to_string language hint) ]
      ]
    |> CCList.pure
  in
  let value = CCFun.flip (CCOption.map_or ~default:"") custom_field in
  let field_type_opt = CCOption.map field_type custom_field in
  let input_by_lang ?required =
    input_by_lang ?required ?flash_fetcher language tenant_languages custom_field
  in
  let name_inputs =
    input_by_lang ~required:true Field.Name (fun lang f ->
      let open CCOption in
      f |> name |> Name.find_opt lang >|= Name.value_name |> value ~default:"")
  in
  let hint_inputs =
    input_by_lang Field.Hint (fun lang f ->
      let open CCOption in
      f |> hint |> Hint.find_opt lang >|= Hint.value_hint |> value ~default:"")
  in
  let validation_subform =
    let current_values = custom_field |> CCOption.map_or ~default:[] validation_strings in
    let rule_input field_type name input_type value disabled =
      let prefixed_name = Format.asprintf "%s[%s]" Field.(show Validation) name in
      let wrapper_class = [ "form-group"; "horizontal"; "flex-gap" ] in
      let input_attributes =
        [ a_input_type input_type; a_name prefixed_name; a_id name; a_value value ]
      in
      let attrs, classes =
        match disabled with
        | true -> a_disabled () :: input_attributes, "disabled" :: wrapper_class
        | false -> input_attributes, wrapper_class
      in
      div
        ~a:[ a_class classes; a_user_data "field-type" (FieldType.show field_type) ]
        [ label ~a:[ a_label_for name ] [ txt (name |> Validation.key_to_human) ]
        ; div ~a:[ a_class [ "input-group" ] ] [ input ~a:attrs () ]
        ]
    in
    let functions =
      {js|
        var select = document.querySelector("[name='field_type']");
        select.addEventListener("change", function(e) {
          var type = e.currentTarget.value;
          var validations = document.querySelectorAll(".form-group[data-field-type]");
          var hints = document.querySelectorAll(".help[data-field-type]");
          validations.forEach(function(elm){
            if(elm.dataset.fieldType != type) {
              elm.classList.add("disabled")
            } else {
              elm.classList.remove("disabled")
            }
            var input = elm.querySelector('input');
            input.disabled = elm.dataset.fieldType != type;
          })
          hints.forEach(function(elm){
            if(elm.dataset.fieldType != type) {
              elm.classList.add("hidden")
            } else {
              elm.classList.remove("hidden")
            }
          })
        })
    |js}
    in
    div
      [ div
          ~a:[ a_class [ "flexcolumn"; "stack" ] ]
          (CCList.map
             (fun (key, input_type, validation_type) ->
                let value =
                  CCList.assoc_opt ~eq:CCString.equal key current_values
                  |> CCOption.value ~default:""
                in
                let disabled =
                  field_type_opt
                  |> CCOption.map_or ~default:false (fun t ->
                    FieldType.equal validation_type t |> not)
                in
                rule_input validation_type key input_type value disabled)
             Validation.all)
      ; script (Unsafe.data functions)
      ]
  in
  let select_options_html =
    let empty = txt "" in
    match custom_field with
    | None -> empty
    | Some m ->
      (match m with
       | Select (_, options) | MultiSelect (_, options) ->
         let list =
           if CCList.is_empty options
           then
             [ p
                 [ Utils.text_to_string language (I18n.NoEntries Field.CustomFieldOptions)
                   |> txt
                 ]
             ]
           else
             [ p
                 [ Utils.hint_to_string
                     language
                     I18n.(CustomFieldSort Field.CustomFieldOptions)
                   |> HttpUtils.add_line_breaks
                 ]
             ; form
                 ~a:
                   [ a_method `Post
                   ; a_action
                       (Sihl.Web.externalize_path
                          (Url.Field.detail_path (model m, id m)
                           |> Format.asprintf "%s/sort-options"))
                   ; a_class [ "stack" ]
                   ; a_user_data "detect-unsaved-changes" ""
                   ]
                 (CCList.cons
                    (CCList.map
                       (fun option ->
                          [ td [ txt (SelectOption.name language option) ]
                          ; td
                              [ txt
                                  (if CCOption.is_some option.SelectOption.published_at
                                   then Utils.field_to_string language Field.PublishedAt
                                   else "")
                              ]
                          ; td
                              [ input
                                  ~a:
                                    [ a_input_type `Hidden
                                    ; a_name Field.(CustomFieldOption |> array_key)
                                    ; a_value SelectOption.(Id.value option.id)
                                    ]
                                  ()
                              ]
                          ; td
                              ~a:[ a_class [ "flexrow"; "justify-end" ] ]
                              [ Url.Option.edit_path
                                  (model m, id m)
                                  option.SelectOption.id
                                |> Component.Input.edit_link
                              ]
                          ])
                       options
                     |> Component.Sortable.create_table ~classnames:[ "table"; "simple" ]
                    )
                    [ div
                        ~a:[ a_class [ "flexrow" ] ]
                        [ submit_element
                            ~classnames:[ "push" ]
                            language
                            Control.UpdateOrder
                            ~submit_type:`Primary
                            ()
                        ]
                    ; csrf_element csrf ()
                    ])
             ]
         in
         div
           [ div
               ~a:[ a_class [ "flexrow"; "flex-gap"; "justify-between"; "align-center" ] ]
               [ div
                   [ h2
                       ~a:[ a_class [ "heading-2"; "has-gap" ] ]
                       [ txt
                           (Field.CustomFieldOption
                            |> Utils.field_to_string language
                            |> CCString.capitalize_ascii)
                       ]
                   ]
               ; div
                   [ link_as_button
                       ~style:`Success
                       ~icon:Icon.Create
                       ~classnames:[ "small" ]
                       ~control:(language, Control.(Add (Some Field.CustomFieldOption)))
                       (Url.Option.new_path (model m, id m))
                   ]
               ]
           ; div
               ~a:[ a_class [ "stack"; "gap" ] ]
               (Component.Notification.create
                  language
                  `Warning
                  [ txt
                      (Utils.hint_to_string language I18n.CustomFieldOptionsCompleteness)
                  ]
                :: list)
           ]
       | Boolean _ | Date _ | Number _ | Text _ -> empty)
  in
  let field_type_selector =
    match custom_field with
    | Some field when CCOption.is_some (Custom_field.published_at field) ->
      let field_type = Custom_field.field_type field in
      selector
        language
        Field.FieldType
        FieldType.show
        [ field_type ]
        (Some field_type)
        ~read_only:true
        ~option_formatter:FieldType.to_string
        ()
    | Some _ | None ->
      selector
        language
        Field.FieldType
        FieldType.show
        FieldType.all
        field_type_opt
        ~option_formatter:FieldType.to_string
        ~add_empty:true
        ~required:true
        ?flash_fetcher
        ()
  in
  let field_type_hints =
    I18n.
      [ CustomFieldTypeText, FieldType.Text
      ; CustomFieldTypeSelect, FieldType.Select
      ; CustomFieldTypeMultiSelect, FieldType.MultiSelect
      ]
    |> CCList.map (fun (hint, field_type) ->
      let hidden =
        let published =
          custom_field
          |> CCOption.map_or ~default:false (Custom_field.published_at %> CCOption.is_some)
        in
        let equal_type =
          field_type_opt |> CCOption.map_or ~default:false FieldType.(equal field_type)
        in
        if published || not equal_type then [ "hidden" ] else []
      in
      div
        ~a:
          [ a_class ("help" :: hidden)
          ; a_user_data "field-type" (FieldType.show field_type)
          ]
        [ txt (Utils.hint_to_string language hint) ])
  in
  [ form
      ~a:
        [ a_method `Post
        ; a_action (Sihl.Web.externalize_path action)
        ; a_class [ "stack-lg" ]
        ; a_user_data "detect-unsaved-changes" ""
        ]
      [ csrf_element csrf ()
      ; div
          ~a:[ a_class [ "grid-col-2" ] ]
          [ div ~a:[ a_class [ "stack-xs" ] ] (field_type_selector :: field_type_hints)
          ; Group.(
              selector
                language
                Field.CustomFieldGroup
                Group.(fun (g : t) -> g.id |> Id.value)
                groups
                Custom_field.(
                  let open CCOption in
                  custom_field
                  >>= group_id
                  >>= fun id ->
                  CCList.find_opt Group.(fun (g : t) -> Id.equal g.id id) groups)
                ~option_formatter:(name language)
                ~add_empty:true
                ?flash_fetcher
                ())
          ; div
              [ h4
                  ~a:[ a_class [ "heading-3" ] ]
                  [ txt
                      (Field.Name
                       |> Utils.field_to_string language
                       |> CCString.capitalize_ascii)
                  ]
              ; div ~a:[ a_class [ "stack" ] ] name_inputs
              ]
          ; div
              [ h4
                  ~a:[ a_class [ "heading-3" ] ]
                  [ txt
                      (Field.Hint
                       |> Utils.field_to_string language
                       |> CCString.capitalize_ascii)
                  ]
              ; div ~a:[ a_class [ "stack" ] ] hint_inputs
              ]
          ; div
              [ h4
                  ~a:[ a_class [ "heading-3" ] ]
                  [ txt (I18n.Validation |> Utils.text_to_string language) ]
              ; validation_subform
              ]
          ]
      ; div
          ~a:[ a_class [ "stack" ] ]
          [ h4
              ~a:[ a_class [ "heading-3" ] ]
              [ txt
                  (Field.Admin
                   |> Utils.field_to_string language
                   |> CCString.capitalize_ascii)
              ]
          ; input_element
              language
              `Text
              Field.AdminHint
              ~orientation:`Horizontal
              ~value:(value (admin_hint %> CCOption.map_or ~default:"" AdminHint.value))
              ?flash_fetcher
          ; div
              ~a:[ a_class [ "grid-col-2" ] ]
              [ input_element
                  ~additional_attributes:
                    DuplicateWeighting.
                      [ a_input_min (`Number min); a_input_max (`Number max) ]
                  ~hints:[ I18n.CustomFieldDuplicateWeight ]
                  language
                  `Number
                  Field.DuplicateWeighting
                  ~orientation:`Vertical
                  ~value:
                    (value
                       CCOption.(
                         fun field ->
                           duplicate_weighting field
                           >|= DuplicateWeighting.value
                           |> map_or ~default:"" CCInt.to_string))
                  ?flash_fetcher
              ]
          ; checkbox_element
              Field.Override
              ~hints:[ I18n.CustomFieldAdminOverride ]
              ?append_html:
                (if
                   custom_field
                   |> CCOption.map_or
                        ~default:false
                        (admin_override %> AdminOverride.value)
                 then Some (warning I18n.CustomFieldAdminOverrideUpdate)
                 else None)
              (admin_override %> AdminOverride.value)
          ; checkbox_element
              ~disabled:
                (custom_field
                 |> CCOption.map_or ~default:false (admin_view_only %> AdminViewOnly.value)
                )
              ~hints:[ I18n.CustomFieldAdminInputOnly ]
              Field.AdminInputOnly
              (admin_input_only %> AdminInputOnly.value)
          ; checkbox_element
              ~hints:[ I18n.CustomFieldAdminViewOnly ]
              Field.AdminViewOnly
              (admin_view_only %> AdminViewOnly.value)
          ; checkbox_element
              ~hints:[ I18n.CustomFieldPromptOnRegistration ]
              Field.PromptOnRegistration
              (prompt_on_registration %> PromptOnRegistration.value)
          ; checkbox_element
              ~disabled:
                (custom_field
                 |> CCOption.map_or ~default:false (fun f ->
                   f |> admin_input_only |> AdminInputOnly.value))
              Field.Required
              (required %> Required.value)
          ; checkbox_element Field.Disabled (disabled %> Disabled.value)
          ; div
              ~a:[ a_class [ "flexrow" ] ]
              [ div
                  ~a:[ a_class [ "push"; "flexrow"; "flex-gap-lg" ] ]
                  [ reset_form_button language
                  ; submit_element
                      language
                      Control.(
                        let field = Some Field.CustomField in
                        match custom_field with
                        | None -> Create field
                        | Some _ -> Update field)
                      ~submit_type:`Primary
                      ()
                  ]
              ]
          ]
      ; (Format.asprintf
           {js|
              var adminViewOnly = document.querySelector("[name='%s']");
              var adminInputOnly = document.querySelector("[name='%s']");
              var required = document.querySelector("[name='%s']");
              var fieldType = document.querySelector("[name='%s']");

              function triggerEvent(elm, name) {
                var event = document.createEvent("HTMLEvents");
                event.initEvent(name, false, true);
                elm.dispatchEvent(event);
              }

              adminInputOnly.addEventListener("change", function(e) {
                required.disabled = e.currentTarget.checked;
              })

              adminViewOnly.addEventListener("change", function(e) {
                adminInputOnly.disabled = e.currentTarget.checked;
                if(e.currentTarget.checked) {
                  adminInputOnly.checked = true;
                  triggerEvent(adminInputOnly, 'change');
                }
              })
         |js}
           Field.(show AdminViewOnly)
           Field.(show AdminInputOnly)
           Field.(show Required)
           Field.(show FieldType)
         |> fun js -> script (Unsafe.data js))
      ]
  ; select_options_html
  ]
;;

let field_buttons language csrf current_model field =
  let open Custom_field in
  let open Pool_common in
  let action field appendix =
    Url.Field.detail_path (current_model, field |> id)
    |> (fun base -> Format.asprintf "%s/%s" base appendix)
    |> Sihl.Web.externalize_path
  in
  let make_form ?disabled_reason action msg submit_type confirmable =
    let button_attribs =
      match disabled_reason with
      | None -> []
      | Some err -> [ a_disabled (); a_title Utils.(error_to_string language err) ]
    in
    form
      ~a:
        [ a_action action
        ; a_method `Post
        ; a_user_data "confirmable" (Utils.confirmable_to_string language confirmable)
        ]
      [ csrf_element csrf ()
      ; submit_element ~attributes:button_attribs language msg ~submit_type ()
      ]
  in
  match field with
  | None -> txt ""
  | Some field ->
    (match published_at field with
     | Some published_at ->
       div
         [ txt
             (Utils.field_to_string language Field.PublishedAt
              |> CCString.capitalize_ascii)
         ; txt ": "
         ; txt (published_at |> PublishedAt.value |> Pool_model.Time.formatted_date_time)
         ]
     | None ->
       let disable_publish =
         Custom_field.has_options field
         |> function
         | Ok () -> None
         | Error err -> Some err
       in
       div
         ~a:[ a_class [ "flexrow"; "flex-gap" ] ]
         [ make_form
             (action field "delete")
             Control.(Delete (Some Field.CustomField))
             `Error
             I18n.DeleteCustomField
         ; make_form
             ?disabled_reason:disable_publish
             (action field "publish")
             Control.(Publish (Some Field.CustomField))
             `Success
             I18n.PublishCustomField
         ])
;;

let detail
      ?custom_field
      current_model
      (Pool_context.{ language; csrf; flash_fetcher; _ } as context)
      groups
      sys_languages
  =
  let changelog_html =
    match custom_field with
    | None -> txt ""
    | Some field ->
      let url =
        let id = Custom_field.id field in
        HttpUtils.Url.Admin.custom_fields_path current_model ~suffix:"changelog" ~id ()
        |> Uri.of_string
      in
      Component.Changelog.list context url None
  in
  let button_form = field_buttons language csrf current_model custom_field in
  div
    ~a:[ a_class [ "trim"; "safety-margin" ] ]
    [ Partials.form_title language Field.CustomField custom_field
    ; div
        ~a:[ a_class [ "flexrow"; "flex-gap"; "justify-between"; "align-center" ] ]
        [ model_subtitle language current_model; button_form ]
    ; div
        ~a:[ a_class [ "stack-lg"; "gap-lg" ] ]
        (field_form
           ?custom_field
           current_model
           ?flash_fetcher
           context
           groups
           sys_languages
         @ [ changelog_html ])
    ]
;;

let index
      field_list
      group_list
      current_model
      Pool_context.({ language; csrf; _ } as context)
  =
  let open Pool_common in
  let grouped, ungrouped = Custom_field.group_fields group_list field_list in
  let thead =
    (Field.[ Title; CustomFieldGroup; PublishedAt ] |> Table.fields_to_txt language)
    @ [ link_as_button
          ~style:`Success
          ~icon:Icon.Add
          ~control:(language, Control.(Add (Some Field.CustomField)))
          (Url.Field.new_path current_model)
      ]
  in
  let field_name field =
    let open Custom_field in
    field |> name |> Name.find_opt_or language "-"
  in
  let hint =
    let open I18n in
    let open Custom_field.Model in
    match current_model with
    | Contact -> CustomFieldContactModel
  in
  let rows =
    let open Custom_field in
    let open CCOption in
    let field_row group field =
      [ txt (field |> field_name)
      ; txt (group >|= Group.name language |> value ~default:"")
      ; txt
          (field
           |> published_at
           |> CCOption.map_or
                ~default:""
                CCFun.(PublishedAt.value %> Pool_model.Time.formatted_date))
      ; Url.Field.edit_path (model field, id field) |> edit_link
      ]
    in
    CCList.flat_map
      (fun (group, fields) -> CCList.map (field_row (Some group)) fields)
      grouped
    @ CCList.map (field_row None) ungrouped
  in
  let sort_ungrouped =
    let sort_form =
      if CCList.is_empty ungrouped
      then
        [ p [ Utils.text_to_string language (I18n.NoEntries Field.CustomFields) |> txt ] ]
      else
        [ p
            [ Utils.hint_to_string language I18n.(CustomFieldSort Field.CustomFields)
              |> HttpUtils.add_line_breaks
            ]
        ; form
            ~a:
              [ a_class [ "stack"; "gap" ]
              ; a_method `Post
              ; a_action
                  (Sihl.Web.externalize_path (Url.index_path current_model)
                   |> Format.asprintf "%s/sort-fields")
              ; a_user_data "detect-unsaved-changes" ""
              ]
            [ csrf_element csrf ()
            ; CCList.map
                (fun field ->
                   div
                     ~a:[ a_class [ "flexrow"; "align-center" ] ]
                     [ txt (field |> field_name)
                     ; input
                         ~a:
                           [ a_input_type `Hidden
                           ; a_name Field.(CustomField |> array_key)
                           ; a_value Custom_field.(field |> id |> Id.value)
                           ]
                         ()
                     ])
                ungrouped
              |> Component.Sortable.create_sortable
            ; div
                ~a:[ a_class [ "flexrow" ] ]
                [ submit_element
                    ~classnames:[ "push"; "small" ]
                    language
                    Control.UpdateOrder
                    ~submit_type:`Primary
                    ()
                ]
            ]
        ]
    in
    div
      (h3
         ~a:[ a_class [ "heading-3"; "has-gap" ] ]
         [ txt (Utils.text_to_string language I18n.SortUngroupedFields) ]
       :: sort_form)
  in
  let groups_html =
    let list =
      if CCList.is_empty group_list
      then
        [ p
            [ Utils.text_to_string language (I18n.NoEntries Field.CustomFieldGroups)
              |> txt
            ]
        ]
      else
        [ p
            [ Utils.hint_to_string language (I18n.CustomFieldSort Field.CustomFieldGroups)
              |> txt
            ]
        ; form
            ~a:
              [ a_method `Post
              ; a_action
                  (Url.Group.index_path current_model
                   |> Format.asprintf "%s/group/sort"
                   |> Sihl.Web.externalize_path)
              ; a_class [ "stack" ]
              ; a_user_data "detect-unsaved-changes" ""
              ]
            [ CCList.map
                (fun group ->
                   let open Custom_field in
                   div
                     ~a:
                       [ a_class
                           [ "flexrow"; "flex-gap"; "justify-between"; "align-center" ]
                       ]
                     [ div [ txt Group.(group |> name language) ]
                     ; div
                         [ input
                             ~a:
                               [ a_input_type `Hidden
                               ; a_name Field.(CustomFieldGroup |> array_key)
                               ; a_value Group.(Id.value group.id)
                               ]
                             ()
                         ]
                     ; Url.Group.edit_path Group.(group.model, group.id)
                       |> edit_link ~classnames:[ "small" ]
                     ])
                group_list
              |> Component.Sortable.create_sortable
            ; div
                ~a:[ a_class [ "flexrow" ] ]
                [ submit_element
                    ~classnames:[ "push"; "small" ]
                    language
                    Control.UpdateOrder
                    ~submit_type:`Primary
                    ()
                ; csrf_element csrf ()
                ]
            ]
        ]
    in
    div
      ~a:[ a_class [ "stack" ] ]
      ([ div
           ~a:[ a_class [ "flexrow"; "justify-between"; "align-center" ] ]
           [ h2
               ~a:[ a_class [ "heading-3" ] ]
               [ txt
                   (Field.CustomFieldGroup
                    |> Utils.field_to_string language
                    |> CCString.capitalize_ascii)
               ]
           ; link_as_button
               ~style:`Success
               ~icon:Icon.Add
               ~classnames:[ "small" ]
               ~control:(language, Control.(Add (Some Field.CustomFieldGroup)))
               (Url.Group.new_path current_model)
           ]
       ; p
           [ Utils.hint_to_string language I18n.CustomFieldGroups
             |> HttpUtils.add_line_breaks
           ]
       ]
       @ list)
  in
  [ div
      ~a:[ a_class [ "stack-lg" ] ]
      [ Table.horizontal_table `Striped ~thead ~align_last_end:true rows
      ; groups_html
      ; sort_ungrouped
      ]
  ]
  |> Layout.CustomField.create ~hint context current_model
;;
