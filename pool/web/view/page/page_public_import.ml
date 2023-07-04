open Tyxml.Html
open Component.Input
open Pool_common.I18n
module HttpUtils = Http_utils
module Message = Pool_common.Message

let txt_to_string lang m = txt (Pool_common.Utils.text_to_string lang m)

let import_confirmation
  Pool_context.{ language; query_language; csrf; _ }
  token
  password_policy
  terms_and_conditions
  =
  let action =
    "/import-confirmation"
    |> HttpUtils.externalize_path_with_lang query_language
  in
  div
    ~a:[ a_class [ "trim"; "narrow"; "safety-margin" ] ]
    [ h1
        ~a:[ a_class [ "heading-1" ] ]
        [ txt_to_string language ImportConfirmationTitle ]
    ; p [ txt_to_string language ImportConfirmationNote ]
    ; form
        ~a:[ a_action action; a_method `Post; a_class [ "stack" ] ]
        [ csrf_element csrf ()
        ; input_element
            language
            `Hidden
            Message.Field.Token
            ~value:(User_import.Token.value token)
        ; input_element
            ~help:
              (Pool_common.I18n.I18nText
                 (password_policy |> I18n.content_to_string))
            ~required:true
            language
            `Password
            Message.Field.Password
        ; input_element
            ~required:true
            language
            `Password
            Message.Field.PasswordConfirmation
        ; Component.Partials.terms_and_conditions_checkbox
            language
            terms_and_conditions
        ; div
            ~a:[ a_class [ "flexrow" ] ]
            [ submit_element
                ~classnames:[ "push" ]
                language
                Message.(Save (Some Field.password))
                ()
            ]
        ]
    ]
;;
