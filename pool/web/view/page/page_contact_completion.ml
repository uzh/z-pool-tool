open Tyxml.Html
module Message = Pool_common.Message

let custom_field_to_input ?flash_fetcher language custom_field =
  let open Custom_field in
  let open CCOption in
  let label = Public.to_common_field language custom_field in
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
      label
  in
  match[@warning "-27"] custom_field with
  | Public.Boolean (_, answer) ->
    answer
    >|= (fun a -> a.Answer.value)
    |> fun value ->
    Component.checkbox_element
      ~as_switch:true
      ~orientation:`Horizontal
      ?value
      language
      label
  | Public.MultiSelect (_, answer, options) ->
    failwith "TODO"
    (* let value = answer |> CCOption.map_or ~default:[] (fun a ->
       a.Answer.value) in Htmx.multi_select ?help ~required
       ~option_formatter:SelectOption.(name language) language label
       SelectOption.show_id options value () *)
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
      label
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
