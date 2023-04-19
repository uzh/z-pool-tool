open CCFun
open Tyxml.Html
module Field = Pool_common.Message.Field

let signup
  terms
  Pool_context.{ language; query_language; csrf; _ }
  flash_fetcher
  =
  let open Component.Input in
  let open Pool_common in
  let submit_url =
    Http_utils.externalize_path_with_lang query_language "/signup"
  in
  let txt_to_string = Utils.text_to_string language %> txt in
  let terms_accepted_name = Field.(show TermsAccepted) in
  let modal_id = "terms-modal" in
  div
    ~a:[ a_class [ "trim"; "narrow"; "safety-margin" ] ]
    [ h1 [ txt_to_string I18n.SignUpTitle ]
    ; form
        ~a:[ a_action submit_url; a_method `Post; a_class [ "stack" ] ]
        [ csrf_element csrf ()
        ; input_element
            language
            `Email
            Field.Email
            ~required:true
            ~flash_fetcher
        ; input_element
            language
            `Text
            Field.Firstname
            ~required:true
            ~flash_fetcher
        ; input_element
            language
            `Text
            Field.Lastname
            ~required:true
            ~flash_fetcher
        ; input_element
            language
            `Password
            Field.Password
            ~required:true
            ~value:""
        ; div
            [ Settings.TermsAndConditions.Terms.value terms
              |> HttpUtils.add_line_breaks
              |> Component.Modal.create
                   language
                   (flip Utils.field_to_string Message.Field.TermsAndConditions
                    %> CCString.capitalize_ascii)
                   modal_id
            ; div
                ~a:[ a_class [ "form-group" ] ]
                [ div
                    [ input
                        ~a:
                          [ a_required ()
                          ; a_value "true"
                          ; a_input_type `Checkbox
                          ; a_id terms_accepted_name
                          ; a_name terms_accepted_name
                          ]
                        ()
                    ; label
                        ~a:[ a_label_for terms_accepted_name ]
                        (Component.Partials.terms_and_conditions_label
                           language
                           modal_id)
                    ]
                ]
            ]
        ; div
            ~a:[ a_class [ "flexrow" ] ]
            [ submit_element ~classnames:[ "push" ] language Message.SignUp () ]
        ]
    ]
;;
