open Tyxml.Html
open Component
module Message = Pool_common.Message

let list csrf translation_list Pool_context.{ language; _ } =
  let input_element = input_element language in
  let build_translations_row translation_list =
    CCList.map
      (fun (key, translations) ->
        let translations_html =
          CCList.map
            (fun translation ->
              let action =
                Sihl.Web.externalize_path
                  (Format.asprintf
                     "/admin/i18n/%s"
                     (translation |> I18n.id |> Pool_common.Id.value))
              in
              form
                ~a:
                  [ a_action action
                  ; a_method `Post
                  ; a_class [ "flex-box"; "flex--row"; "flex--align-end" ]
                  ]
                [ Component.csrf_element csrf ()
                ; input_element
                    `Text
                    (Some "content")
                    (translation
                    |> I18n.language
                    |> Pool_common.Language.field_name_of_t)
                    (translation |> I18n.content |> I18n.Content.value)
                ; submit_element
                    language
                    Message.(Update (Some Message.translation))
                    ~classnames:[ "button--primary" ]
                    ()
                ])
            translations
        in
        div
          [ h2
              [ txt
                  (key
                  |> I18n.Key.value
                  |> CCString.replace ~which:`All ~sub:"_" ~by:" "
                  |> CCString.capitalize_ascii)
              ]
          ; div ~a:[ a_class [ "stack" ] ] translations_html
          ])
      translation_list
  in
  let translations = build_translations_row translation_list in
  div
    [ h1 [ txt Pool_common.(Utils.text_to_string Language.En I18n.I18nTitle) ]
    ; div ~a:[ a_class [ "stack" ] ] translations
    ]
;;
