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
  form
    ~a:[ a_action (action |> externalize); a_method `Post; a_class [ "stack" ] ]
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
        language
        ~flash_fetcher
        ~required:true
        ~value:(value (fun t -> t.email_text |> EmailText.value))
        Field.EmailText
    ; textarea_element
        language
        ~flash_fetcher
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
;;

let edit ({ Pool_context.language; _ } as context) template flash_fetcher =
  let open Message_template in
  let action =
    match template with
    | None -> "/admin/message-template/"
    | Some template ->
      template.id |> Id.value |> Format.asprintf "/admin/message-template/%s/"
  in
  div
    ~a:[ a_class [ "trim"; "safety-margin" ] ]
    [ Component.Partials.form_title language Field.MessageTemplate template
    ; template_form context template action flash_fetcher
    ]
;;
