open Tyxml.Html
module Message = Pool_common.Message

let form
  Pool_context.{ language; csrf; query_language; _ }
  flash_fetcher
  custom_fields
  =
  let custom_fields_form =
    let to_html field =
      Component.custom_field_to_input ~flash_fetcher language field
    in
    Page_contact_edit.grouped_custom_fields_form language custom_fields to_html
  in
  div
    ~a:[ a_class [ "trim"; "safety-margin" ] ]
    [ h1
        ~a:[ a_class [ "heading-1" ] ]
        [ txt
            Pool_common.(
              Utils.text_to_string language I18n.ProfileCompletionTitle)
        ]
    ; form
        ~a:
          [ a_class [ "stack" ]
          ; a_method `Post
          ; a_action
              (Http_utils.externalize_path_with_lang
                 query_language
                 "/user/completion")
          ]
        Component.(
          (csrf_element csrf () :: custom_fields_form)
          @ [ submit_element
                language
                Message.(Save None)
                ~submit_type:`Primary
                ()
            ])
    ]
;;
