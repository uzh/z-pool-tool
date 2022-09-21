open Tyxml.Html
open Component
module Message = Pool_common.Message

let base_path = "/admin/custom-fields"

let form
  ?custom_field
  Pool_context.{ language; csrf; _ }
  sys_languages
  flash_fetcher
  =
  let open Custom_field in
  let action =
    match custom_field with
    | None -> base_path
    | Some f -> Format.asprintf "%s/%s" base_path (f.id |> Id.value)
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
  let input_by_lang ?(required = false) field value_fnc =
    let open Pool_common in
    let group_class = Component.Elements.group_class [] `Horizontal in
    CCList.map
      (fun lang ->
        let label_text =
          Pool_common.(
            lang
            |> Language.field_of_t
            |> Utils.field_to_string language
            |> CCString.capitalize_ascii)
        in
        let id =
          Format.asprintf
            "%s-%s"
            (Message.Field.show field)
            (Language.show lang)
        in
        let input_element =
          let attrs =
            [ a_input_type `Text
            ; a_id id
            ; a_name
                (Format.asprintf
                   "%s[%s]"
                   (Message.Field.array_key field)
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
      sys_languages
  in
  let name_inputs =
    input_by_lang ~required:true Pool_common.Message.Field.Name (fun lang ->
      let open CCOption in
      custom_field
      >>= (fun f -> Name.find_opt f.name lang)
      >|= Name.value_name
      |> value ~default:"")
  in
  let hint_inputs =
    input_by_lang Pool_common.Message.Field.Hint (fun lang ->
      let open CCOption in
      custom_field
      >>= (fun f -> Hint.find_opt f.hint lang)
      >|= Hint.value_hint
      |> value ~default:"")
  in
  form
    ~a:
      [ a_method `Post
      ; a_action (Sihl.Web.externalize_path action)
      ; a_class [ "stack-lg" ]
      ]
    [ Component.csrf_element csrf ()
    ; div
        ~a:[ a_class [ "switcher"; "flex-gap" ] ]
        [ Component.selector
            language
            Pool_common.Message.Field.Model
            Model.show
            Model.all
            (CCOption.map (fun f -> f.model) custom_field)
            ~add_empty:true
            ~required:true
            ~flash_fetcher
            ()
        ; Component.selector
            language
            Pool_common.Message.Field.FieldType
            FieldType.show
            FieldType.all
            (CCOption.map (fun f -> f.field_type) custom_field)
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
            [ txt Pool_common.(I18n.Validation |> Utils.text_to_string language)
            ]
        ; input_element
            language
            `Text
            Pool_common.Message.Field.Regex
            ~orientation:`Horizontal
            ~value:
              (value (fun f ->
                 f.validation.Validation.regex |> Validation.Regex.value))
            ~required:true
            ~flash_fetcher
        ; Component.selector
            language
            Pool_common.Message.Field.ErrorMessage
            Validation.Error.show
            Validation.Error.all
            (CCOption.map (fun f -> f.validation.Validation.error) custom_field)
            ~option_formatter:Validation.Error.format_as_label
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
                  Message.Field.Admin
                  |> Utils.field_to_string language
                  |> CCString.capitalize_ascii)
            ]
        ; input_element
            language
            `Text
            Pool_common.Message.Field.AdminHint
            ~orientation:`Horizontal
            ~value:
              (value (fun f ->
                 f.admin.Admin.hint
                 |> CCOption.map_or ~default:"" Admin.Hint.value))
            ~flash_fetcher
        ; checkbox_element Pool_common.Message.Field.Overwrite (fun f ->
            f.admin.Admin.overwrite |> Admin.Overwrite.value)
        ]
    ; div
        ~a:[ a_class [ "stack" ] ]
        [ checkbox_element Pool_common.Message.Field.Required (fun f ->
            f.required |> Required.value)
        ; checkbox_element Pool_common.Message.Field.Disabled (fun f ->
            f.disabled |> Disabled.value)
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
    ~a:[ a_class [ "trim"; "safety-margin"; "measure"; "stack" ] ]
    [ h1 [ txt title ]; form ?custom_field context sys_languages flash_fetcher ]
;;

let index field_list Pool_context.{ language; _ } =
  let thead = Pool_common.Message.Field.[ Some Title; None ] in
  let rows =
    let open Custom_field in
    CCList.map
      (fun field ->
        [ txt
            (Name.find_opt field.name language
            |> CCOption.map_or ~default:"-" Name.value_name)
        ; a
            ~a:
              [ a_href
                  (Sihl.Web.externalize_path
                     (Format.asprintf
                        "%s/%s/edit"
                        base_path
                        (field.id |> Custom_field.Id.value)))
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
