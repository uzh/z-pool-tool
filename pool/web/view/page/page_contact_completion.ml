open Tyxml.Html
module Message = Pool_common.Message

let custom_field_to_input ?flash_fetcher language custom_field =
  let open Custom_field in
  let open CCOption in
  let field = Public.to_common_field language custom_field in
  let help = Public.to_common_hint language custom_field in
  let required = Public.required custom_field |> Required.value in
  let create input_type value =
    Component.input_element
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
    answer
    >|= (fun a -> a.Answer.value)
    |> fun value ->
    Component.checkbox_element
      ~as_switch:true
      ~orientation:`Horizontal
      ?value
      language
      field
  | Public.MultiSelect (_, options, answers) ->
    let selected = CCList.map (fun { Answer.value; _ } -> value) answers in
    let t =
      Component.
        { options
        ; selected
        ; to_label = SelectOption.name language
        ; to_value = SelectOption.show_id
        ; additional_attributes = None
        }
    in
    Component.multi_select language t field ()
  | Public.Number (_, answer) ->
    answer >|= (fun a -> a.Answer.value |> CCInt.to_string) |> create `Number
  | Public.Text (_, answer) ->
    answer >|= (fun a -> a.Answer.value) |> create `Text
  | Public.Select (_, options, answer) ->
    let value = answer >|= fun a -> a.Answer.value in
    Component.selector
      ?flash_fetcher
      ?help
      ~required
      ~option_formatter:SelectOption.(name language)
      ~add_empty:true
      language
      field
      SelectOption.show_id
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
