open Tyxml.Html
module Input = Component_input

let input_name key input_type =
  Format.asprintf "%s[%s]" (key |> Filter.show_key) input_type
;;

let filter language experiment =
  let open Filter in
  (* TODO[timhub]: add name *)
  let operator_select key =
    all_operators
    |> CCList.map (fun o ->
           option
             ~a:[ a_value (o |> Stringify.any_operator_to_string) ]
             (o
             |> Stringify.any_operator_to_string
             |> CCString.capitalize_ascii
             |> txt))
    |> select ~a:[ a_name (input_name key "operator") ]
    |> CCList.pure
    |> div ~a:[ a_class [ "select" ] ]
    |> CCList.pure
    |> div ~a:[ a_class [ "form-group" ] ]
  in
  let filter_label key =
    div
      [ txt
          (key
          |> key_to_field
          |> Pool_common.Utils.field_to_string language
          |> CCString.capitalize_ascii)
      ]
  in
  let wrap_filter html = div ~a:[ a_class [ "switcher"; "flex-gap" ] ] html in
  let string_filter key =
    [ filter_label key
    ; div [ operator_select key ]
    ; div
        ~a:[ a_class [ "form-group" ] ]
        [ input ~a:[ a_input_type `Text; a_name (input_name key "value") ] () ]
    ]
    |> wrap_filter
  in
  let date_filter key =
    [ filter_label key
    ; div [ operator_select key ] (* TODO [timhub]: implement from / to *)
    ; div
        ~a:[ a_class [ "stack" ] ]
        [ div
            ~a:[ a_class [ "form-group" ] ]
            [ Input.flatpicker_element
                language
                `Datetime_local
                Pool_common.Message.Field.From
            ]
        ; div
            ~a:[ a_class [ "form-group" ] ]
            [ Input.flatpicker_element
                language
                `Datetime_local
                Pool_common.Message.Field.To
            ]
        ]
    ]
    |> wrap_filter
  in
  let boolean_filter key =
    [ filter_label key
    ; div [ operator_select key ]
      (* TODO[timhub]: maybe add toggle:
         https://www.w3schools.com/howto/howto_css_switch.asp *)
    ; Input.checkbox_element language (key_to_field key)
    ]
    |> wrap_filter
  in
  let _ =
    form
      ~a:
        [ a_method `Post
        ; a_action
            (Format.asprintf
               "/admin/experiments/%s/invitations/contacts"
               (experiment.Experiment.id |> Pool_common.Id.value)
            |> Sihl.Web.externalize_path)
        ; a_class [ "flexcolumn"; "stack" ]
        ]
      (CCList.map
         (fun (key, input_type) ->
           match input_type with
           | `Str -> string_filter key
           | `Date -> date_filter key
           | `Bool -> boolean_filter key)
         keys_with_types
      @ [ Component_input.submit_element
            language
            (Pool_common.Message.Create None)
            ()
        ])
  in
  div
    [ h3 ~a:[ a_class [ "heading-3" ] ] [ txt "Filter contacts" ]
    ; div ~a:[ a_class [ "gap" ] ] [ Unsafe.node "contact-filter" [] ]
    ]
;;
