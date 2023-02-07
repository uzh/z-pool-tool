open Tyxml.Html
module Input = Component.Input
module Message = Pool_common.Message

let custom_field_to_input ?flash_fetcher language custom_field =
  let open Custom_field in
  let open CCOption in
  let field = Public.to_common_field language custom_field in
  let help = Public.to_common_hint language custom_field in
  let required = Public.required custom_field |> Required.value in
  let create input_type value =
    Input.input_element
      ?flash_fetcher
      ?value
      ?help
      ~required
      language
      input_type
      field
  in
  match custom_field with
  | Public.Boolean (_, answer) ->
    Input.checkbox_element
      ~as_switch:true
      ~orientation:`Horizontal
      ?value:(answer >>= Answer.value)
      language
      field
  | Public.MultiSelect (_, options, answer) ->
    let selected = answer >>= Answer.value |> CCOption.value ~default:[] in
    let t =
      Input.
        { options
        ; selected
        ; to_label = SelectOption.Public.name language
        ; to_value = SelectOption.Public.show_id
        }
    in
    Input.multi_select language t field ()
  | Public.Number (_, answer) ->
    answer >>= Answer.value >|= CCInt.to_string |> create `Number
  | Public.Text (_, answer) -> answer >>= Answer.value |> create `Text
  | Public.Select (_, options, answer) ->
    let value = answer >>= Answer.value in
    Input.selector
      ?flash_fetcher
      ?help
      ~required
      ~option_formatter:SelectOption.Public.(name language)
      ~add_empty:true
      language
      field
      SelectOption.Public.show_id
      options
      value
      ()
;;

let form
  Pool_context.{ language; csrf; query_language; _ }
  flash_fetcher
  custom_fields
  =
  let custom_fields_form =
    let to_html field = custom_field_to_input ~flash_fetcher language field in
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
    ; p
        [ Pool_common.(Utils.text_to_string language I18n.ProfileCompletionText)
          |> Http_utils.add_line_breaks
        ]
    ; form
        ~a:
          [ a_class [ "stack" ]
          ; a_method `Post
          ; a_action
              (Http_utils.externalize_path_with_lang
                 query_language
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
                    Message.(Save None)
                    ~submit_type:`Primary
                    ()
                ]
            ])
    ]
;;
