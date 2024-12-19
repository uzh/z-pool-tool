open Tyxml.Html

let answer_to_html
      ?(add_data_label = false)
      user
      language
      (custom_field : Custom_field.Public.t)
  =
  let open CCFun in
  let open Custom_field in
  let open Public in
  let answer_to_html to_html =
    let map_or = CCOption.map_or ~default:(txt "") in
    function
    | None -> txt ""
    | Some { Custom_field.Answer.value; admin_value; _ } ->
      let admin_input =
        match admin_value, Pool_context.user_is_admin user with
        | Some _, false | None, (true | false) -> txt ""
        | Some admin_value, true ->
          span
            ~a:[ a_class [ "flexcolumn" ] ]
            [ strong
                [ i
                    [ txt
                        Pool_common.(
                          Utils.field_to_string language Pool_message.Field.AdminInput
                          |> CCString.capitalize_ascii)
                    ]
                ]
            ; to_html admin_value
            ]
      in
      let attribs =
        match add_data_label with
        | false -> []
        | true ->
          [ a_user_data "label" Custom_field.Public.(name_value language custom_field) ]
      in
      span
        ~a:([ a_class [ "flexcolumn"; "stack-xs" ] ] @ attribs)
        [ value |> CCOption.map to_html |> map_or CCFun.(CCList.return %> span)
        ; admin_input
        ]
  in
  let select_option_to_html = SelectOption.Public.name language %> txt in
  match custom_field with
  | Boolean (_, answer) ->
    answer_to_html (Pool_common.Utils.bool_to_string language %> txt) answer
  | Date (_, answer) -> answer_to_html (Utils.Ptime.date_to_human %> txt) answer
  | MultiSelect (_, _, answer) ->
    answer_to_html
      (CCList.map select_option_to_html %> CCList.intersperse (br ()) %> span)
      answer
  | Number (_, answer) -> answer_to_html (CCInt.to_string %> txt) answer
  | Select (_, _, answer) -> answer_to_html select_option_to_html answer
  | Text (_, answer) -> answer_to_html txt answer
;;
