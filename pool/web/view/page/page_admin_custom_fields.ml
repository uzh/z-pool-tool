open Tyxml.Html
open Component
module Message = Pool_common.Message

let base_path = "/admin/custom-fields"

let make_option_url field option path =
  let open Custom_field in
  Format.asprintf
    "%s/%s/options/%s/%s"
    base_path
    (id field |> Id.value)
    (option.SelectOption.id |> SelectOption.Id.value)
    path
  |> Sihl.Web.externalize_path
;;

let input_by_lang ?(required = false) language tenant_languages field value_fnc =
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
      let input_element =
        let attrs =
          [ a_input_type `Text
          ; a_id id
          ; a_name
              (Format.asprintf
                 "%s[%s]"
                 (Message.Field.show field)
                 (Language.show lang))
          ; a_value (value_fnc lang)
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

let form
  ?(custom_field : Custom_field.t option)
  Pool_context.{ language; csrf; _ }
  tenant_languages
  flash_fetcher
  =
  let open Custom_field in
  let action =
    match custom_field with
    | None -> base_path
    | Some f -> Format.asprintf "%s/%s" base_path (f |> id |> Id.value)
  in
  let checkbox_element ?orientation ?help ?(default = false) field fnc =
    checkbox_element
      language
      ?orientation
      ?help
      field
      ~value:(custom_field |> CCOption.map_or ~default fnc)
      ~flash_fetcher
  in
  let value = CCFun.flip (CCOption.map_or ~default:"") custom_field in
  let field_type = CCOption.map field_type custom_field in
  let input_by_lang ?required =
    input_by_lang ?required language tenant_languages
  in
  let name_inputs =
    input_by_lang ~required:true Message.Field.Name (fun lang ->
      let open CCOption in
      custom_field
      >|= (fun f -> name f)
      >>= Name.find_opt lang
      >|= Name.value_name
      |> value ~default:"")
  in
  let hint_inputs =
    input_by_lang Message.Field.Hint (fun lang ->
      let open CCOption in
      custom_field
      >|= (fun f -> hint f)
      >>= Hint.find_opt lang
      >|= Hint.value_hint
      |> value ~default:"")
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
                 field_type
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
       | Select (_, options) ->
         let make_link url control =
           let url =
             Format.asprintf
               "%s/%s/options/%s"
               base_path
               (m |> id |> Id.value)
               url
             |> Sihl.Web.externalize_path
           in
           a
             ~a:[ a_href url ]
             [ txt Pool_common.(control |> Utils.control_to_string language) ]
         in
         let list =
           form
             ~a:
               [ a_method `Post
               ; a_action
                   (Format.asprintf
                      "%s/%s/sort-options"
                      base_path
                      (m |> id |> Id.value)
                   |> Sihl.Web.externalize_path)
               ; a_class [ "stack" ]
               ]
             (CCList.cons
                (div
                   ~a:[ a_user_data "sortable" "" ]
                   (CCList.map
                      (fun option ->
                        div
                          ~a:
                            [ a_class
                                [ "flexrow"
                                ; "flex-gap"
                                ; "justify-between"
                                ; "align-center"
                                ]
                            ; a_user_data "sortable-item" ""
                            ; a_draggable true
                            ]
                          [ div [ txt (SelectOption.name language option) ]
                          ; div
                              [ input
                                  ~a:
                                    [ a_input_type `Hidden
                                    ; a_name
                                        Message.Field.(
                                          CustomFieldOption |> array_key)
                                    ; a_value SelectOption.(Id.value option.id)
                                    ]
                                  ()
                              ]
                          ; div
                              ~a:
                                [ a_class
                                    [ "flexrow"; "flex-gap"; "align-center" ]
                                ]
                              [ a
                                  ~a:
                                    [ a_href (make_option_url m option "edit") ]
                                  [ txt
                                      Pool_common.(
                                        Message.(Edit None)
                                        |> Utils.control_to_string language)
                                  ]
                              ]
                          ])
                      options))
                [ csrf_element csrf ()
                ; submit_element
                    language
                    Message.UpdateOrder
                    ~submit_type:`Success
                    ()
                ])
         in
         div
           [ h2
               ~a:[ a_class [ "heading-2" ] ]
               [ txt
                   (Message.Field.CustomFieldOption
                   |> Pool_common.Utils.field_to_string language
                   |> CCString.capitalize_ascii)
               ]
           ; p [ make_link "new" Message.(Add (Some Field.CustomFieldOption)) ]
           ; list
           ]
       | Number _ | Text _ -> empty)
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
              Message.Field.Model
              Model.show
              Model.all
              (CCOption.map model custom_field)
              ~add_empty:true
              ~required:true
              ~flash_fetcher
              ()
          ; selector
              language
              Message.Field.FieldType
              FieldType.show
              FieldType.all
              field_type
              ~add_empty:true
              ~required:true
              ~flash_fetcher
              ()
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
          ; checkbox_element Message.Field.AdminInputOnly (fun f ->
              (f |> admin).Admin.input_only |> Admin.InputOnly.value)
          ; checkbox_element
              ~help:Pool_common.I18n.CustomFieldAdminViewOnly
              Message.Field.AdminViewOnly
              (fun f -> (f |> admin).Admin.view_only |> Admin.ViewOnly.value)
          ; (Format.asprintf
               {js|
                var toggle = document.querySelector("[name='%s']");
                var target = document.querySelector("[name='%s']");
                toggle.addEventListener("change", function(e) {
                  if(e.currentTarget.checked) {
                    target.checked = true;
                  }
                })
              |js}
               Message.Field.(show AdminViewOnly)
               Message.Field.(show AdminInputOnly)
            |> fun js -> script (Unsafe.data js))
          ]
      ; div
          ~a:[ a_class [ "stack" ] ]
          [ checkbox_element Message.Field.Required (fun f ->
              f |> required |> Required.value)
          ; checkbox_element Message.Field.Disabled (fun f ->
              f |> disabled |> Disabled.value)
          ; submit_element
              language
              Message.(
                let field = Some Field.CustomField in
                match custom_field with
                | None -> Create field
                | Some _ -> Update field)
              ~submit_type:`Success
              ()
          ]
      ]
  ; select_options_html
  ]
;;

let detail
  ?custom_field
  (Pool_context.{ language; _ } as context)
  sys_languages
  flash_fetcher
  =
  let title =
    Pool_common.(
      Utils.control_to_string
        language
        Message.(
          if CCOption.is_none custom_field
          then Create (Some Field.CustomField)
          else Update (Some Field.CustomField)))
  in
  div
    ~a:[ a_class [ "trim"; "safety-margin"; "measure" ] ]
    [ h1 [ txt title ]
    ; div
        ~a:[ a_class [ "stack-lg" ] ]
        (form ?custom_field context sys_languages flash_fetcher)
    ]
;;

let index field_list Pool_context.{ language; _ } =
  let thead = Message.Field.[ Some Title; None ] in
  let rows =
    let open Custom_field in
    CCList.map
      (fun field ->
        [ txt
            (field
            |> name
            |> Name.find_opt language
            |> CCOption.map_or ~default:"-" Name.value_name)
        ; a
            ~a:
              [ a_href
                  (Sihl.Web.externalize_path
                     (Format.asprintf
                        "%s/%s/edit"
                        base_path
                        (field |> id |> Id.value)))
              ]
            [ txt Pool_common.(Message.More |> Utils.control_to_string language)
            ]
        ])
      field_list
  in
  div
    ~a:[ a_class [ "trim"; "safety-margin"; "measure" ] ]
    [ h1
        ~a:[ a_class [ "heading-1" ] ]
        [ txt Pool_common.(Utils.nav_link_to_string language I18n.CustomFields)
        ]
    ; p
        [ a
            ~a:
              [ a_href
                  (Sihl.Web.externalize_path
                     (Format.asprintf "%s/new" base_path))
              ]
            [ txt
                Pool_common.(
                  Message.(Add (Some Field.CustomField))
                  |> Utils.control_to_string language)
            ]
        ]
    ; div
        ~a:[ a_class [ "stack" ] ]
        [ Table.horizontal_table `Striped language ~thead rows ]
    ]
;;
