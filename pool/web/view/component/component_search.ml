open CCFun
open Tyxml.Html
module Field = Pool_common.Message.Field
module I18n = Pool_common.I18n

let hidden_input name decode =
  CCOption.map_or ~default:[] (fun value ->
    [ input
        ~a:[ a_hidden (); a_name (Field.show name); a_value (decode value) ]
        ()
    ])
;;

let input_element
  item_to_html
  placeholder
  input_field
  ?(disabled = false)
  ?(value = "")
  ?results
  action
  =
  let result_list =
    let wrap =
      div
        ~a:
          [ a_class
              [ "flexcolumn"
              ; "gap-sm"
              ; "striped"
              ; "bg-white"
              ; "inset-sm"
              ; "border"
              ; "border-radius"
              ; "hide-empty"
              ]
          ]
      %> CCList.return
    in
    match results with
    | None -> []
    | Some [] -> [ txt "No results found" ] |> wrap
    | Some results -> results |> CCList.map item_to_html |> wrap
  in
  let attrs =
    [ a_input_type `Text
    ; a_value value
    ; a_name Field.(show input_field)
    ; a_class [ "query-input" ]
    ; a_placeholder placeholder
    ]
  in
  let attrs = if disabled then a_disabled () :: attrs else attrs in
  input
    ~a:
      (attrs
       @ Component_utils.htmx_attribs
           ~action
           ~trigger:"keyup changed delay:1s"
           ~target:"closest [data-query='input']"
           ())
    ()
  :: result_list
  |> div ~a:[ a_class [ "flexcolumn" ]; a_user_data "query" "input" ]
;;

let create
  item_to_html
  placeholder
  input_field
  field_label
  ?(current = [])
  ?disabled
  ?role
  ?exclude_roles_of
  ?value
  ?results
  language
  path
  =
  let search_role = hidden_input Field.Role Role.Actor.show role in
  let exclude_roles_of =
    hidden_input Field.ExcludeRolesOf Admin.(Id.value) exclude_roles_of
  in
  div
    ~a:[ a_class [ "form-group" ]; a_user_data "query" "wrapper" ]
    ([ label [ txt Pool_common.(Utils.nav_link_to_string language field_label) ]
     ; input_element
         item_to_html
         placeholder
         input_field
         ?disabled
         ?value
         ?results
         path
     ; div
         ~a:[ a_user_data "query" "results"; a_class [ "hide-empty" ] ]
         (CCList.map item_to_html current)
     ]
     @ search_role
     @ exclude_roles_of)
;;

module Experiment = struct
  let item (id, title) =
    let open Experiment in
    div
      ~a:[ a_user_data "id" (Id.value id); a_class [ "has-icon"; "inset-xs" ] ]
      [ Component_icon.(to_html ~classnames:[ "toggle-item" ] CloseCircle)
      ; span [ txt (Title.value title) ]
      ; input
          ~a:
            [ a_input_type `Checkbox
            ; a_class [ "hidden" ]
            ; a_name Field.(array_key Value)
            ; a_value (Id.value id)
            ; a_checked ()
            ]
          ()
      ]
  ;;

  let placeholder = "Search by experiment title"
  let input_element = input_element item placeholder Field.Title
  let create = create item placeholder Field.Title I18n.Experiments
end

module Location = struct
  let item (id, name) =
    let open Pool_location in
    div
      ~a:[ a_user_data "id" (Id.value id); a_class [ "has-icon"; "inset-xs" ] ]
      [ Component_icon.(to_html ~classnames:[ "toggle-item" ] CloseCircle)
      ; span [ txt (Name.value name) ]
      ; input
          ~a:
            [ a_input_type `Checkbox
            ; a_class [ "hidden" ]
            ; a_name Field.(array_key Value)
            ; a_value (Id.value id)
            ; a_checked ()
            ]
          ()
      ]
  ;;

  let placeholder = "Search by location name"
  let input_element = input_element item placeholder Field.Name
  let create = create item placeholder Field.Name I18n.Locations
end
