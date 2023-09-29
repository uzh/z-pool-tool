open Tyxml.Html
open Component.Input
module Field = Pool_common.Message.Field

let build_add_button label path =
  let open Message_template in
  a
    ~a:[ a_class [ "btn"; "primary" ]; a_href (Sihl.Web.externalize_path path) ]
    [ txt (Format.asprintf "Add %s" (Label.to_human label)) ]
;;

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
  (template : Message_template.t option)
  flash_fetcher
  =
  let value = CCFun.flip (CCOption.map_or ~default:"") template in
  let open Message_template in
  let language_select =
    match languages with
    | None -> div []
    | Some languages ->
      let open Pool_common.Language in
      selector ~required:true language Field.Language show languages None ()
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

let template_form
  ({ Pool_context.language; query_language; csrf; _ } as context)
  ?(hide_text_message_input = false)
  ?languages
  ?text_elements
  (template : Message_template.t option)
  action
  flash_fetcher
  =
  let externalize = Http_utils.externalize_path_with_lang query_language in
  let submit =
    let open Pool_common.Message in
    let field = Field.MessageTemplate |> CCOption.pure in
    match template with
    | None -> Create field
    | Some _ -> Update field
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
             template
             flash_fetcher)
       @ [ div
             ~a:[ a_class [ "flexrow" ] ]
             [ submit_element ~classnames:[ "push" ] language submit () ]
         ])
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
