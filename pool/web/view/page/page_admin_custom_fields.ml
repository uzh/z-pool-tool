open Tyxml.Html
open Component.Input
module Table = Component.Table
module Partials = Component.Partials
module Message = Pool_common.Message

module Url = struct
  open Custom_field

  let concat = CCString.concat "/"
  let fallback_path = "/admin/custom-fields"
  let index_path m = [ fallback_path; Model.show m ] |> concat

  module Field = struct
    let field_key = "field"
    let create_path m = [ index_path m; field_key ] |> concat
    let new_path m = [ index_path m; field_key; "new" ] |> concat

    let detail_path (m, id) =
      [ index_path m; field_key; id |> Id.value ] |> concat
    ;;

    let edit_path (m, id) = [ detail_path (m, id); "edit" ] |> concat
  end

  module Option = struct
    open SelectOption

    let options_key = "options"
    let index_path = Field.detail_path
    let new_path field = [ index_path field; options_key; "new" ] |> concat
    let create_path field = [ Field.detail_path field; options_key ] |> concat

    let detail_path field id =
      [ index_path field; options_key; id |> Id.value ] |> concat
    ;;

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
    [ p
        [ strong
            [ txt
                (Format.asprintf
                   "%s: "
                   Pool_common.(
                     Utils.field_to_string language Message.Field.Model
                     |> CCString.capitalize_ascii))
            ]
        ; txt (Custom_field.Model.show model |> CCString.capitalize_ascii)
        ]
    ]
;;

let custom_fields_layout language current_model html =
  let open Custom_field in
  let subnav_links =
    Model.(
      all
      |> CCList.map (fun f ->
           ( f |> show |> CCString.capitalize_ascii
           , f |> Url.index_path
           , equal current_model f )))
  in
  let html =
    [ h2
        ~a:[ a_class [ "heading-2" ] ]
        [ txt (current_model |> Model.show |> CCString.capitalize_ascii) ]
    ; html
    ]
  in
  div
    ~a:[ a_class [ "trim"; "safety-margin"; "measure" ] ]
    [ h1
        ~a:[ a_class [ "heading-1" ] ]
        [ txt Pool_common.(Utils.nav_link_to_string language I18n.CustomFields)
        ]
    ; Component.Navigation.make_tabs html subnav_links
    ]
;;

let input_by_lang
  ?(required = false)
  language
  tenant_languages
  flash_fetcher
  elm
  field
  value_fnc
  =
  let open Pool_common in
  let group_class = Elements.group_class [] `Horizontal in
  CCList.map
    (fun lang ->
      let required =
        required
        && CCList.mem ~eq:Pool_common.Language.equal lang tenant_languages
      in
      let label_text =
        lang
        |> Language.field_of_t
        |> Utils.field_to_string language
        |> CCString.capitalize_ascii
      in
      let id =
        Format.asprintf "%s-%s" (Message.Field.show field) (Language.show lang)
      in
      let value =
        let open CCOption in
        flash_fetcher
          (Format.asprintf
             "%s[%s]"
             (Message.Field.show field)
             (Language.show lang))
        <+> (elm >|= value_fnc lang)
        |> value ~default:""
      in
      let input_element =
        let attrs =
          [ a_input_type `Text
          ; a_id id
          ; a_name
              (Format.asprintf
                 "%s[%s]"
                 (Message.Field.show field)
                 (Language.show lang))
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
            [ txt
                (if required
                then Format.asprintf "%s *" label_text
                else label_text)
            ]
        ; input_element
        ])
    Pool_common.Language.all
;;

let field_form
  ?(custom_field : Custom_field.t option)
  current_model
  Pool_context.{ language; csrf; _ }
  groups
  tenant_languages
  flash_fetcher
  =
  let open Custom_field in
  let action =
    match custom_field with
    | None -> Url.Field.create_path current_model
    | Some f -> Url.Field.detail_path (model f, id f)
  in
  let checkbox_element
    ?(disabled = false)
    ?orientation
    ?help
    ?(default = false)
    field
    fnc
    =
    checkbox_element
      language
      ~additional_attributes:(if disabled then [ a_disabled () ] else [])
      ?orientation
      ?help
      field
      ~value:(custom_field |> CCOption.map_or ~default fnc)
      ~flash_fetcher
  in
  let value = CCFun.flip (CCOption.map_or ~default:"") custom_field in
  let field_type_opt = CCOption.map field_type custom_field in
  let input_by_lang ?required =
    input_by_lang ?required language tenant_languages flash_fetcher custom_field
  in
  let name_inputs =
    input_by_lang ~required:true Message.Field.Name (fun lang f ->
      let open CCOption in
      f |> name |> Name.find_opt lang >|= Name.value_name |> value ~default:"")
  in
  let hint_inputs =
    input_by_lang Message.Field.Hint (fun lang f ->
      let open CCOption in
      f |> hint |> Hint.find_opt lang >|= Hint.value_hint |> value ~default:"")
  in
  let validation_subform =
    let current_values =
      custom_field
      |> CCOption.map_or ~default:[] (fun f -> f |> validation_strings)
    in
    let rule_input field_type name input_type value disabled =
      let prefixed_name =
        Format.asprintf "%s[%s]" Message.Field.(show Validation) name
      in
      let wrapper_class = [ "switcher"; "flex-gap"; "align-center" ] in
      let input_attributes =
        [ a_input_type input_type
        ; a_name prefixed_name
        ; a_id name
        ; a_value value
        ; a_class [ "grow-2" ]
        ]
      in
      let attrs, classes =
        match disabled with
        | true -> a_disabled () :: input_attributes, "disabled" :: wrapper_class
        | false -> input_attributes, wrapper_class
      in
      div
        ~a:
          [ a_class classes
          ; a_user_data "field-type" (FieldType.show field_type)
          ]
        [ div
            ~a:[ a_class [ "grow" ] ]
            [ label ~a:[ a_label_for name ] [ txt name ] ]
        ; input ~a:attrs ()
        ]
    in
    let functions =
      {js|
        var select = document.querySelector("[name='field_type']");
        select.addEventListener("change", function(e) {
          var type = e.currentTarget.value;
          var inputs = document.querySelectorAll("[data-field-type]");
          inputs.forEach(function(elm){
            if(elm.dataset.fieldType != type) {
              elm.classList.add("disabled")
            } else {
              elm.classList.remove("disabled")
            }
            var input = elm.querySelector('input');
            input.disabled = elm.dataset.fieldType != type;
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
           form
             ~a:
               [ a_method `Post
               ; a_action
                   (Sihl.Web.externalize_path
                      (Url.Field.detail_path (model m, id m)
                      |> Format.asprintf "%s/sort-options"))
               ; a_class [ "stack" ]
               ]
             (CCList.cons
                (tablex
                   ~a:[ a_class [ "table"; "simple"; "sortable" ] ]
                   [ tbody
                       ~a:[ a_user_data "sortable" "" ]
                       (CCList.map
                          (fun option ->
                            tr
                              ~a:[ a_user_data "sortable-item" "" ]
                              [ td [ txt (SelectOption.name language option) ]
                              ; td
                                  [ txt
                                      (if CCOption.is_some
                                            option.SelectOption.published_at
                                      then
                                        Pool_common.(
                                          Utils.field_to_string
                                            language
                                            Message.Field.PublishedAt)
                                      else "")
                                  ]
                              ; td
                                  [ input
                                      ~a:
                                        [ a_input_type `Hidden
                                        ; a_name
                                            Message.Field.(
                                              CustomFieldOption |> array_key)
                                        ; a_value
                                            SelectOption.(Id.value option.id)
                                        ]
                                      ()
                                  ]
                              ; td
                                  ~a:[ a_class [ "flexrow"; "justify-end" ] ]
                                  [ a
                                      ~a:
                                        [ a_href
                                            (Url.Option.edit_path
                                               (model m, id m)
                                               option.SelectOption.id
                                            |> Sihl.Web.externalize_path)
                                        ]
                                      [ txt
                                          Pool_common.(
                                            Message.(Edit None)
                                            |> Utils.control_to_string language)
                                      ]
                                  ]
                              ])
                          options)
                   ])
                [ csrf_element csrf ()
                ; div
                    ~a:[ a_class [ "flexrow" ] ]
                    [ submit_element
                        ~classnames:[ "push" ]
                        language
                        Message.UpdateOrder
                        ~submit_type:`Primary
                        ()
                    ]
                ])
         in
         div
           [ div
               ~a:
                 [ a_class
                     [ "flexrow"
                     ; "flex-gap"
                     ; "justify-between"
                     ; "align-center"
                     ]
                 ]
               [ div
                   [ h2
                       ~a:[ a_class [ "heading-2" ] ]
                       [ txt
                           (Message.Field.CustomFieldOption
                           |> Pool_common.Utils.field_to_string language
                           |> CCString.capitalize_ascii)
                       ]
                   ]
               ; div
                   [ link_as_button
                       ~style:`Success
                       ~icon:`CreateOutline
                       ~classnames:[ "small" ]
                       ~control:
                         (language, Message.(Add (Some Field.CustomFieldOption)))
                       (Url.Option.new_path (model m, id m))
                   ]
               ]
           ; div ~a:[ a_class [ "gap" ] ] [ list ]
           ]
       | Boolean _ | Number _ | Text _ -> empty)
  in
  [ form
      ~a:
        [ a_method `Post
        ; a_action (Sihl.Web.externalize_path action)
        ; a_class [ "stack-lg" ]
        ]
      [ csrf_element csrf ()
      ; div
          ~a:[ a_class [ "switcher"; "flex-gap" ] ]
          [ selector
              language
              Message.Field.FieldType
              FieldType.show
              FieldType.all
              field_type_opt
              ~option_formatter:FieldType.to_string
              ~add_empty:true
              ~required:true
              ~flash_fetcher
              ()
          ; Group.(
              selector
                language
                Message.Field.CustomFieldGroup
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
                ~flash_fetcher
                ())
          ]
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
      ; div
          ~a:[ a_class [ "stack" ] ]
          [ h4
              ~a:[ a_class [ "heading-4" ] ]
              [ txt
                  Pool_common.(
                    Message.Field.Hint
                    |> Utils.field_to_string language
                    |> CCString.capitalize_ascii)
              ]
          ; div ~a:[ a_class [ "stack" ] ] hint_inputs
          ]
      ; div
          ~a:[ a_class [ "stack" ] ]
          [ h4
              ~a:[ a_class [ "heading-4" ] ]
              [ txt
                  Pool_common.(I18n.Validation |> Utils.text_to_string language)
              ]
          ]
      ; validation_subform
      ; div
          ~a:[ a_class [ "stack" ] ]
          [ h4
              ~a:[ a_class [ "heading-4" ] ]
              [ txt
                  Pool_common.(
                    Message.Field.Admin
                    |> Utils.field_to_string language
                    |> CCString.capitalize_ascii)
              ]
          ; input_element
              language
              `Text
              Message.Field.AdminHint
              ~orientation:`Horizontal
              ~value:
                (value (fun f ->
                   (f |> admin).Admin.hint
                   |> CCOption.map_or ~default:"" Admin.Hint.value))
              ~flash_fetcher
          ; checkbox_element Message.Field.Overwrite (fun f ->
              (f |> admin).Admin.overwrite |> Admin.Overwrite.value)
          ; checkbox_element
              ~disabled:
                (custom_field
                |> CCOption.map_or ~default:false (fun f ->
                     (f |> admin).Admin.view_only |> Admin.ViewOnly.value))
              ~help:Pool_common.I18n.CustomFieldAdminInputOnly
              Message.Field.AdminInputOnly
              (fun f -> (f |> admin).Admin.input_only |> Admin.InputOnly.value)
          ; checkbox_element
              ~help:Pool_common.I18n.CustomFieldAdminViewOnly
              Message.Field.AdminViewOnly
              (fun f -> (f |> admin).Admin.view_only |> Admin.ViewOnly.value)
          ]
      ; div
          ~a:[ a_class [ "stack" ] ]
          [ checkbox_element
              ~disabled:
                (custom_field
                |> CCOption.map_or ~default:false (fun f ->
                     (f |> admin).Admin.input_only |> Admin.InputOnly.value
                     || FieldType.(equal (f |> field_type) MultiSelect)))
              Message.Field.Required
              (fun f -> f |> required |> Required.value)
          ; checkbox_element Message.Field.Disabled (fun f ->
              f |> disabled |> Disabled.value)
          ; div
              ~a:[ a_class [ "flexrow" ] ]
              [ submit_element
                  ~classnames:[ "push" ]
                  language
                  Message.(
                    let field = Some Field.CustomField in
                    match custom_field with
                    | None -> Create field
                    | Some _ -> Update field)
                  ~submit_type:`Primary
                  ()
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

              fieldType.addEventListener("change", function(e) {
                required.disabled = (e.currentTarget.value === "%s")
              })
         |js}
           Message.Field.(show AdminViewOnly)
           Message.Field.(show AdminInputOnly)
           Message.Field.(show Required)
           Message.Field.(show FieldType)
           FieldType.(show MultiSelect)
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
  let make_form action msg submit_type confirmable =
    form
      ~a:
        [ a_action action
        ; a_method `Post
        ; a_user_data
            "confirmable"
            (Pool_common.Utils.confirmable_to_string language confirmable)
        ]
      [ csrf_element csrf (); submit_element language msg ~submit_type () ]
  in
  match field with
  | None -> txt ""
  | Some field ->
    (match published_at field with
     | Some published_at ->
       div
         [ txt
             (Utils.field_to_string language Message.Field.PublishedAt
             |> CCString.capitalize_ascii)
         ; txt ": "
         ; txt
             (published_at
             |> PublishedAt.value
             |> Utils.Time.formatted_date_time)
         ]
     | None ->
       div
         ~a:[ a_class [ "flexrow"; "flex-gap" ] ]
         [ make_form
             (action field "delete")
             Message.(Delete (Some Field.CustomField))
             `Error
             I18n.DeleteCustomField
         ; make_form
             (action field "publish")
             Message.(Publish (Some Field.CustomField))
             `Success
             I18n.PublisCustomField
         ])
;;

let detail
  ?custom_field
  current_model
  (Pool_context.{ language; csrf; _ } as context)
  groups
  sys_languages
  flash_fetcher
  =
  let button_form = field_buttons language csrf current_model custom_field in
  div
    ~a:[ a_class [ "trim"; "safety-margin"; "measure" ] ]
    [ Partials.form_title language Message.Field.CustomField custom_field
    ; div
        ~a:
          [ a_class [ "flexrow"; "flex-gap"; "justify-between"; "align-center" ]
          ]
        [ model_subtitle language current_model; button_form ]
    ; div
        ~a:[ a_class [ "stack-lg" ] ]
        (field_form
           ?custom_field
           current_model
           context
           groups
           sys_languages
           flash_fetcher)
    ]
;;

let index field_list group_list current_model Pool_context.{ language; csrf; _ }
  =
  let grouped, ungrouped = Custom_field.group_fields group_list field_list in
  let thead =
    (Message.Field.[ Title; CustomFieldGroup; PublishedAt ]
    |> Table.fields_to_txt language)
    @ [ link_as_button
          ~style:`Success
          ~icon:`Add
          ~control:(language, Message.(Add (Some Field.CustomField)))
          (Url.Field.new_path current_model)
      ]
  in
  let field_name field =
    let open Custom_field in
    field |> name |> Name.find_opt_or language "-"
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
               CCFun.(
                 PublishedAt.value
                 %> Ptime.to_date
                 %> Pool_common.Utils.Time.formatted_date))
      ; a
          ~a:
            [ a_href
                (Url.Field.edit_path (model field, id field)
                |> Sihl.Web.externalize_path)
            ]
          [ txt Pool_common.(Message.More |> Utils.control_to_string language) ]
      ]
    in
    CCList.flat_map
      (fun (group, fields) -> CCList.map (field_row (Some group)) fields)
      grouped
    @ CCList.map (field_row None) ungrouped
  in
  let sort_ungrouped =
    div
      [ h3
          ~a:[ a_class [ "heading-3" ] ]
          [ txt
              Pool_common.(
                Utils.text_to_string language I18n.SortUngroupedFields)
          ]
      ; form
          ~a:
            [ a_class [ "stack" ]
            ; a_method `Post
            ; a_action
                (Sihl.Web.externalize_path (Url.index_path current_model)
                |> Format.asprintf "%s/sort-fields")
            ]
          [ csrf_element csrf ()
          ; div
              ~a:[ a_user_data "sortable" "" ]
              (CCList.map
                 (fun field ->
                   div
                     ~a:
                       [ a_class [ "flexrow"; "align-center"; "inset-sm" ]
                       ; a_user_data "sortable-item" ""
                       ]
                     [ txt (field |> field_name)
                     ; input
                         ~a:
                           [ a_input_type `Hidden
                           ; a_name Message.Field.(CustomField |> array_key)
                           ; a_value Custom_field.(field |> id |> Id.value)
                           ]
                         ()
                     ])
                 ungrouped)
          ; div
              ~a:[ a_class [ "flexrow" ] ]
              [ submit_element
                  ~classnames:[ "push" ]
                  language
                  Message.UpdateOrder
                  ~submit_type:`Primary
                  ()
              ]
          ]
      ]
  in
  let groups_html =
    let list =
      form
        ~a:
          [ a_method `Post
          ; a_action
              (Url.Group.index_path current_model
              |> Format.asprintf "%s/group/sort"
              |> Sihl.Web.externalize_path)
          ; a_class [ "stack" ]
          ]
        (CCList.cons
           (div
              ~a:[ a_user_data "sortable" "" ]
              (CCList.map
                 (fun group ->
                   let open Custom_field in
                   div
                     ~a:
                       [ a_class
                           [ "flexrow"
                           ; "flex-gap"
                           ; "justify-between"
                           ; "align-center"
                           ; "inset-sm"
                           ]
                       ; a_user_data "sortable-item" ""
                       ]
                     [ div [ txt Group.(group |> name language) ]
                     ; div
                         [ input
                             ~a:
                               [ a_input_type `Hidden
                               ; a_name
                                   Message.Field.(CustomFieldGroup |> array_key)
                               ; a_value Group.(Id.value group.id)
                               ]
                             ()
                         ]
                     ; div
                         ~a:
                           [ a_class [ "flexrow"; "flex-gap"; "align-center" ] ]
                         [ a
                             ~a:
                               [ a_href
                                   (Url.Group.edit_path
                                      Group.(group.model, group.id)
                                   |> Sihl.Web.externalize_path)
                               ]
                             [ txt
                                 Pool_common.(
                                   Message.(Edit None)
                                   |> Utils.control_to_string language)
                             ]
                         ]
                     ])
                 group_list))
           [ csrf_element csrf ()
           ; div
               ~a:[ a_class [ "flexrow" ] ]
               [ submit_element
                   ~classnames:[ "push" ]
                   language
                   Message.UpdateOrder
                   ~submit_type:`Primary
                   ()
               ]
           ])
    in
    div
      ~a:[ a_class [ "stack" ] ]
      [ div
          ~a:[ a_class [ "flexrow"; "justify-between"; "align-center" ] ]
          [ h2
              ~a:[ a_class [ "heading-3" ] ]
              [ txt
                  (Message.Field.CustomFieldGroup
                  |> Pool_common.Utils.field_to_string language
                  |> CCString.capitalize_ascii)
              ]
          ; link_as_button
              ~style:`Success
              ~icon:`Add
              ~classnames:[ "small" ]
              ~control:(language, Message.(Add (Some Field.CustomFieldGroup)))
              (Url.Group.new_path current_model)
          ]
      ; list
      ]
  in
  div
    ~a:[ a_class [ "stack-lg" ] ]
    [ Table.horizontal_table `Striped ~thead ~align_last_end:true rows
    ; groups_html
    ; sort_ungrouped
    ]
  |> custom_fields_layout language current_model
;;
