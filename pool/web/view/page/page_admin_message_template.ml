open Tyxml.Html
open Component.Input
module Field = Pool_common.Message.Field

let index { Pool_context.language; _ } templates =
  let open Message_template in
  let thead =
    ([ Field.Label; Field.Language ] |> Component.Table.fields_to_txt language)
    @ [ txt "" ]
  in
  div
    ~a:[ a_class [ "trim"; "safety-margin" ] ]
    [ h1
        ~a:[ a_class [ "heading-1" ] ]
        [ txt
            (Pool_common.(Utils.field_to_string language Field.MessageTemplate)
            |> CCString.capitalize_ascii)
        ]
    ; CCList.map
        (fun template ->
          [ txt (to_human_label template)
          ; txt (template.language |> Pool_common.Language.show)
          ; edit_link
              (template.id
              |> Id.value
              |> Format.asprintf "/admin/message-templates/%s/edit")
          ])
        templates
      |> Component.Table.horizontal_table `Striped ~align_last_end:true ~thead
    ]
;;

let edit { Pool_context.language; query_language; csrf; _ } template =
  let externalize = Http_utils.externalize_path_with_lang query_language in
  let value = CCFun.flip (CCOption.map_or ~default:"") template in
  let open Message_template in
  let action =
    match template with
    | None -> "/admin/message-templates/"
    | Some template ->
      template.id |> Id.value |> Format.asprintf "/admin/message-templates/%s/"
  in
  div
    ~a:[ a_class [ "trim"; "safety-margin" ] ]
    [ form
        ~a:
          [ a_action (action |> externalize)
          ; a_method `Post
          ; a_class [ "stack" ]
          ]
        [ csrf_element csrf ()
        ; input_element
            language
            ~required:true
            ~value:(value (fun t -> t.email_subject |> EmailSubject.value))
            `Text
            Field.EmailSubject
        ; textarea_element
            language
            ~required:true
            ~value:(value (fun t -> t.email_text |> EmailText.value))
            Field.EmailText
        ; textarea_element
            language
            ~required:true
            ~value:(value (fun t -> t.sms_text |> SmsText.value))
            Field.SmsText
        ; div
            ~a:[ a_class [ "flexrow" ] ]
            [ submit_element
                ~classnames:[ "push" ]
                language
                Pool_common.Message.(Update (Some Field.MessageTemplate))
                ()
            ]
        ]
    ]
;;
