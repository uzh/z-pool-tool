open Tyxml.Html
open Component.Input
module Field = Pool_common.Message.Field

let table
  ?(buttons = txt "")
  ?(can_update_experiment = false)
  ?delete_path
  language
  templates
  to_edit_path
  =
  let open Message_template in
  let empty_hint =
    match templates with
    | [] ->
      p
        [ txt
            Pool_common.(
              Utils.text_to_string
                language
                (I18n.NoEntries Field.MessageTemplates))
        ]
    | _ -> txt ""
  in
  let thead =
    ([ Field.Label; Field.Language ] |> Component.Table.fields_to_txt language)
    @ [ buttons ]
  in
  CCList.map
    (fun template ->
      let buttons = edit_link (template |> to_edit_path) in
      let buttons =
        match delete_path with
        | None -> buttons
        | Some (delete_path, csrf) ->
          let delete =
            let action = delete_path template in
            form
              ~a:
                [ a_method `Post
                ; a_action action
                ; a_user_data
                    "confirmable"
                    Pool_common.(
                      Utils.confirmable_to_string
                        language
                        I18n.DeleteMessageTemplate)
                ]
              [ csrf_element csrf ()
              ; submit_element
                  ~has_icon:Icon.TrashOutline
                  ~submit_type:`Error
                  language
                  Pool_common.Message.(Delete None)
                  ()
              ]
          in
          (if can_update_experiment then [ buttons; delete ] else [])
          |> div ~a:[ a_class [ "flexrow"; "flex-gap"; "justify-end" ] ]
      in
      [ txt (to_human_label template)
      ; txt (template.language |> Pool_common.Language.show)
      ; buttons
      ])
    templates
  |> Component.Table.horizontal_table `Striped ~align_last_end:true ~thead
  |> fun table -> div ~a:[ a_class [ "stack" ] ] [ table; empty_hint ]
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
    ; table language templates edit_path
    ]
;;

let template_inputs
  { Pool_context.language; _ }
  ?(hide_text_message_input = false)
  ?languages
  ?fixed_language
  ?selected_language
  (template : Message_template.t option)
  flash_fetcher
  =
  let value = CCFun.flip (CCOption.map_or ~default:"") template in
  let open Message_template in
  let language_select =
    let open Pool_common.Language in
    let languages_select () =
      match languages with
      | None -> div []
      | Some languages ->
        selector
          ~required:true
          language
          Field.Language
          show
          languages
          selected_language
          ()
    in
    fixed_language
    |> CCOption.map_or ~default:(languages_select ()) (fun lang ->
      selector
        ~read_only:true
        language
        Field.Language
        show
        [ lang ]
        (Some lang)
        ())
  in
  let textarea_element ?rich_text ~value =
    textarea_element language ?rich_text ~value ~flash_fetcher ~required:true
  in
  let plain_text_element =
    let id = Field.(show PlainText) in
    div
      ~a:[ a_class [ "form-group" ] ]
      [ div
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
              ; Component.Icon.(to_html RefreshOutline)
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
      ; span
          ~a:[ a_class [ "help" ] ]
          [ Pool_common.(Utils.hint_to_string language I18n.EmailPlainText)
            |> HttpUtils.add_line_breaks
          ]
      ]
  in
  let text_message_input =
    if hide_text_message_input
    then txt ""
    else
      textarea_element
        ~rich_text:false
        ~value:(value (fun t -> t.sms_text |> SmsText.value))
        Field.SmsText
  in
  [ div
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
  ; text_message_input
  ]
;;

type template_form_input =
  | New of Message_template.Label.t
  | Existing of Message_template.t

let template_form
  ({ Pool_context.language; query_language; csrf; _ } as context)
  ?(hide_text_message_input = false)
  ?languages
  ?text_elements
  ?fixed_language
  input
  action
  flash_fetcher
  =
  let open Message_template in
  let externalize = Http_utils.externalize_path_with_lang query_language in
  let submit, template, label =
    let open Pool_common.Message in
    let field = Field.MessageTemplate |> CCOption.pure in
    match input with
    | New label -> Create field, None, label
    | Existing t -> Update field, Some t, t.label
  in
  let hint =
    label
    |> Message_template.template_hint
    |> Pool_common.Utils.hint_to_string language
    |> txt
    |> Component.Collapsible.create_note language
  in
  let text_elements_html =
    text_elements
    |> CCOption.map_or
         ~default:(txt "")
         (Component.MessageTextElements.build_help language)
  in
  let form =
    form
      ~a:
        [ a_action (action |> externalize)
        ; a_method `Post
        ; a_class [ "stack" ]
        ; a_user_data "detect-unsaved-changes" ""
        ]
      ((csrf_element csrf ()
        :: template_inputs
             context
             ~hide_text_message_input
             ?languages
             ?fixed_language
             template
             flash_fetcher)
       @ [ div
             ~a:[ a_class [ "flexrow" ] ]
             [ submit_element ~classnames:[ "push" ] language submit () ]
         ])
  in
  div ~a:[ a_class [ "stack" ] ] [ hint; text_elements_html; form ]
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
    ; template_form
        context
        ~text_elements
        (Existing template)
        action
        flash_fetcher
    ]
;;

let preview_modal_id = "message-template-preview"

let preview_template_modal language (label, templates) =
  let open Message_template in
  let open Pool_common in
  let field_to_string = Utils.field_to_string_capitalized language in
  let html =
    templates
    |> CCList.map
         (fun ({ language; email_subject; email_text; sms_text; _ } : t) ->
            let email_html =
              div
                [ h4 [ txt (field_to_string Field.Email) ]
                ; p
                    ~a:[ a_class [ "border-bottom" ] ]
                    [ strong
                        [ txt
                            (Format.asprintf
                               "%s: %s"
                               (field_to_string Field.EmailSubject)
                               (EmailSubject.value email_subject))
                        ]
                    ]
                ; p [ EmailText.value email_text |> Unsafe.data ]
                ]
            in
            let text_message_html =
              div
                [ h4 [ txt (field_to_string Field.TextMessage) ]
                ; p [ SmsText.value sms_text |> HttpUtils.add_line_breaks ]
                ]
            in
            let _ =
              [ Field.EmailSubject, EmailSubject.value email_subject |> txt
              ; Field.EmailText, EmailText.value email_text |> Unsafe.data
              ; ( Field.SmsText
                , SmsText.value sms_text |> HttpUtils.add_line_breaks )
              ]
              |> Component.Table.vertical_table `Simple language
            in
            div
              [ h3
                  [ txt
                      (Format.asprintf
                         "%s: %s"
                         (field_to_string Field.Language)
                         (Language.show language))
                  ]
              ; div ~a:[ a_class [ "stack" ] ] [ email_html; text_message_html ]
              ])
    |> div ~a:[ a_class [ "stack" ] ]
  in
  Component.Modal.create
    ~active:true
    language
    (fun _ -> Label.show label)
    preview_modal_id
    html
;;

let preview_modal_buttons labels =
  let open Message_template in
  let id = "htmx-" ^ preview_modal_id in
  let modal = div ~a:[ a_id id ] [] in
  labels
  |> CCList.map (fun label ->
    let url =
      label
      |> Label.show
      |> Format.asprintf "/admin/message-template/%s/template-preview"
      |> Sihl.Web.externalize_path
    in
    li
      [ span
          ~a:
            [ a_user_data "hx-get" url
            ; a_user_data "hx-target" ("#" ^ id)
            ; a_user_data "hx-swap" "innerHTML"
            ]
          [ txt (Label.show label) ]
      ])
  |> ul
  |> fun buttons -> div [ modal; buttons ]
;;
