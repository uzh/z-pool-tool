open Tyxml.Html
open Component.Input
module Field = Pool_common.Message.Field

let table language templates create_path to_edit_path =
  let open Message_template in
  let create_button =
    match create_path with
    | None -> txt ""
    | Some path ->
      link_as_button
        ~style:`Success
        ~icon:`Add
        ~control:
          (language, Pool_common.Message.(Add (Some Field.MessageTemplate)))
        path
  in
  let thead =
    ([ Field.Label; Field.Language ] |> Component.Table.fields_to_txt language)
    @ [ create_button ]
  in
  CCList.map
    (fun template ->
      [ txt (to_human_label template)
      ; txt (template.language |> Pool_common.Language.show)
      ; edit_link (template |> to_edit_path)
      ])
    templates
  |> Component.Table.horizontal_table `Striped ~align_last_end:true ~thead
;;

let index { Pool_context.language; _ } templates =
  let edit_path m =
    let open Message_template in
    m.id |> Id.value |> Format.asprintf "/admin/message-template/%s/edit"
  in
  div
    ~a:[ a_class [ "trim"; "safety-margin" ] ]
    [ h1
        ~a:[ a_class [ "heading-1" ] ]
        [ txt
            (Pool_common.(Utils.field_to_string language Field.MessageTemplate)
            |> CCString.capitalize_ascii)
        ]
    ; table language templates None edit_path
    ]
;;

let template_form
  { Pool_context.language; query_language; csrf; _ }
  ?languages
  ?text_elements
  (template : Message_template.t option)
  action
  flash_fetcher
  =
  let externalize = Http_utils.externalize_path_with_lang query_language in
  let value = CCFun.flip (CCOption.map_or ~default:"") template in
  let open Message_template in
  let language_select =
    match languages with
    | None -> div []
    | Some languages ->
      let open Pool_common.Language in
      selector ~required:true language Field.Language show languages None ()
  in
  let textarea_element ?(attributes = []) ?rich_text ~value =
    textarea_element
      language
      ~attributes
      ?rich_text
      ~value
      ~flash_fetcher
      ~required:true
  in
  let submit =
    let open Pool_common.Message in
    let field = Field.MessageTemplate |> CCOption.pure in
    match template with
    | None -> Create field
    | Some _ -> Update field
  in
  let text_elements_html =
    match text_elements with
    | None -> txt ""
    | Some elements ->
      div
        ~a:[ a_class [ "inset"; "border"; "border-radius"; "bg-grey-light" ] ]
        [ p
            [ txt
                Pool_common.(
                  Utils.hint_to_string language I18n.TemplateTextElementsHint)
            ]
        ; elements
          |> CCList.map (fun (label, text) ->
               [ txt (Format.asprintf "{%s}" label)
               ; text |> Http_utils.add_line_breaks
               ])
          |> Component.Table.horizontal_table `Simple ~align_top:true
        ]
  in
  let plain_text_element =
    let id = Field.(show PlainText) in
    div
      ~a:[ a_class [ "form-group" ] ]
      [ p
          [ txt Pool_common.(Utils.hint_to_string language I18n.EmailPlainText)
          ]
      ; div
          ~a:[ a_class [ "flexrow"; "flex-gap"; "justify-between" ] ]
          [ label
              ~a:[ a_label_for id ]
              [ Pool_common.(Utils.field_to_string language Field.PlainText)
                |> CCString.capitalize_ascii
                |> Format.asprintf "%s*"
                |> txt
              ]
          ; div
              ~a:
                [ a_class [ "flexrow"; "flex-gap-sm"; "pointer" ]
                ; a_user_data "toggle-reset-plaintext" Field.(show EmailText)
                ]
              [ txt
                  Pool_common.(
                    Utils.control_to_string language Message.ResetPlainText)
              ; Component.Icon.icon `RefreshOutline
              ]
          ]
      ; textarea
          ~a:
            [ a_name id
            ; a_id id
            ; a_required ()
            ; a_user_data "plain-text-for" Field.(show EmailText)
            ]
          (txt (value (fun t -> t.plain_text |> PlainText.value)))
      ]
  in
  let form =
    form
      ~a:
        [ a_action (action |> externalize)
        ; a_method `Post
        ; a_class [ "stack-lg" ]
        ]
      [ csrf_element csrf ()
      ; div
          ~a:[ a_class [ "switcher"; "flex-gap" ] ]
          [ input_element
              language
              ~flash_fetcher
              ~required:true
              ~value:(value (fun t -> t.email_subject |> EmailSubject.value))
              `Text
              Field.EmailSubject
          ; language_select
          ]
      ; textarea_element
          ~rich_text:true
          ~value:(value (fun t -> t.email_text |> EmailText.value))
          Field.EmailText
      ; plain_text_element
      ; textarea_element
          ~rich_text:false
          ~value:(value (fun t -> t.sms_text |> SmsText.value))
          Field.SmsText
      ; div
          ~a:[ a_class [ "flexrow" ] ]
          [ submit_element ~classnames:[ "push" ] language submit () ]
      ]
  in
  div ~a:[ a_class [ "stack" ] ] [ text_elements_html; form ]
;;

let edit
  ({ Pool_context.language; _ } as context)
  template
  (tenant : Pool_tenant.t)
  flash_fetcher
  =
  let open Message_template in
  let action =
    template.id |> Id.value |> Format.asprintf "/admin/message-template/%s/"
  in
  let text_elements =
    Component.MessageTextElements.message_template_help
      language
      tenant
      template.label
  in
  div
    ~a:[ a_class [ "trim"; "safety-margin" ] ]
    [ Component.Partials.form_title
        language
        Field.MessageTemplate
        (Some template)
    ; template_form context ~text_elements (Some template) action flash_fetcher
    ]
;;
