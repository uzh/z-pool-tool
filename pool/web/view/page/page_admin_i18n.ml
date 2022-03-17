open Tyxml.Html
open Component
module Message = Pool_common.Message

let list csrf translation_list message Pool_context.{ language; _ } =
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
              div
                [ p
                    [ txt
                        (translation
                        |> I18n.language
                        |> Pool_common.Language.code)
                    ]
                ; form
                    ~a:[ a_action action; a_method `Post ]
                    [ Component.csrf_element csrf ()
                    ; input_element
                        `Text
                        (Some "content")
                        Message.Translation
                        (translation |> I18n.content |> I18n.Content.value)
                    ; submit_element
                        language
                        Message.(Update (Some Message.translation))
                    ]
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
          ; div translations_html
          ])
      translation_list
  in
  let translations = build_translations_row translation_list in
  let html =
    div
      [ h1 [ txt Pool_common.(Utils.text_to_string Language.En I18n.I18nTitle) ]
      ; div translations
      ]
  in
  Page_layout.create html message language
;;
