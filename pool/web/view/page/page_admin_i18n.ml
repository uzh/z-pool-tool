open Tyxml.Html
open Component.Input
module Message = Pool_message

let list translation_list Pool_context.{ language; csrf; _ } =
  let input translation input_type =
    let orientation = `Horizontal in
    let label_field = Pool_common.Language.field_of_t (I18n.language translation) in
    let identifier = translation |> I18n.id |> Pool_common.Id.value in
    let value = translation |> I18n.content |> I18n.Content.value in
    let field = Pool_message.Field.translation in
    let textarea_element rich_text =
      textarea_element
        ~rich_text
        ~orientation
        ~classnames:[ "grow" ]
        ~label_field
        ~identifier
        ~required:true
        ~value
        language
        field
    in
    match input_type with
    | `RichText -> textarea_element true
    | `TextArea -> textarea_element false
    | `TextInput ->
      input_element
        ~orientation
        ~classnames:[ "grow" ]
        ~label_field
        ~identifier
        ~required:true
        ~value
        language
        `Text
        field
  in
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
                let text_input = input translation (I18n.Key.input_type key) in
                form
                  ~a:
                    [ a_action action
                    ; a_method `Post
                    ; a_class [ "flexrow"; "flex-gap" ]
                    ; a_user_data "detect-unsaved-changes" ""
                    ]
                  [ csrf_element csrf ()
                  ; text_input
                  ; submit_icon ~classnames:[ "primary" ] Component.Icon.Save
                  ])
             translations
         in
         div
           [ h2
               ~a:[ a_class [ "heading-2" ] ]
               [ txt
                   (key
                    |> I18n.Key.show
                    |> CCString.replace ~which:`All ~sub:"_" ~by:" "
                    |> CCString.capitalize_ascii)
               ]
           ; div ~a:[ a_class [ "stack"; "flexcolumn" ] ] translations_html
           ])
      translation_list
  in
  let translations = build_translations_row translation_list in
  div
    ~a:[ a_class [ "safety-margin"; "trim" ] ]
    [ h1
        ~a:[ a_class [ "heading-1" ] ]
        [ txt Pool_common.(Utils.nav_link_to_string Language.En I18n.I18n) ]
    ; div ~a:[ a_class [ "stack-lg" ] ] translations
    ]
;;
