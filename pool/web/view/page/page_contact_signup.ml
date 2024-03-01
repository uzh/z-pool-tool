open CCFun
open Tyxml.Html
module Field = Pool_message.Field

let signup
  terms
  custom_fields
  Pool_context.{ language; query_language; csrf; _ }
  flash_fetcher
  =
  let open Component.Input in
  let open Pool_common in
  let submit_url =
    Http_utils.externalize_path_with_lang query_language "/signup"
  in
  let txt_to_string = Utils.text_to_string language %> txt in
  let custom_fields_html =
    let to_html field =
      Component.Input.custom_field_to_static_input
        ~force_required:true
        ~flash_fetcher
        language
        field
    in
    custom_fields |> CCList.map to_html
  in
  div
    ~a:[ a_class [ "trim"; "narrow"; "safety-margin" ] ]
    [ h1 [ txt_to_string I18n.SignUpTitle ]
    ; form
        ~a:[ a_action submit_url; a_method `Post; a_class [ "stack" ] ]
        ([ csrf_element csrf ()
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
         ]
         @ custom_fields_html
         @ [ Component.Partials.terms_and_conditions_checkbox language terms
           ; div
               ~a:[ a_class [ "flexrow" ] ]
               [ submit_element
                   ~classnames:[ "push" ]
                   language
                   Pool_message.Control.SignUp
                   ()
               ]
           ])
    ]
;;
