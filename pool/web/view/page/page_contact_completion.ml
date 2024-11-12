open Tyxml.Html
module Input = Component.Input
module Message = Pool_message

let form
  Pool_context.{ language; csrf; query_parameters; _ }
  flash_fetcher
  custom_fields
  =
  let custom_fields_form =
    let to_html field =
      Input.custom_field_to_static_input ~flash_fetcher language field
    in
    Page_contact_edit.grouped_custom_fields_form language custom_fields to_html
  in
  div
    ~a:[ a_class [ "trim"; "safety-margin" ] ]
    [ h1
        ~a:[ a_class [ "heading-1" ] ]
        [ txt
            Pool_common.(
              Utils.text_to_string language I18n.DashboardProfileCompletionTitle)
        ]
    ; Component.Notification.notification
        language
        `Warning
        [ Pool_common.(Utils.text_to_string language I18n.ProfileCompletionText)
          |> Http_utils.add_line_breaks
        ]
    ; form
        ~a:
          [ a_class [ "stack"; "gap-lg" ]
          ; a_method `Post
          ; a_action
              (Http_utils.externalize_path_with_params
                 query_parameters
                 "/user/completion")
          ; a_user_data "detect-unsaved-changes" ""
          ]
        Component.(
          (Input.csrf_element csrf () :: custom_fields_form)
          @ [ div
                ~a:[ a_class [ "flexrow" ] ]
                [ Input.submit_element
                    ~classnames:[ "push" ]
                    language
                    Message.(Control.Save None)
                    ~submit_type:`Primary
                    ()
                ]
            ])
    ]
;;
