open CCFun
open Tyxml.Html
module Field = Pool_common.Message.Field
module I18n = Pool_common.I18n

let query_field = Field.Search

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
  ?(disabled = false)
  ?(value = "")
  ?results
  action
  =
  let result_list =
    let wrap =
      div ~a:[ a_class [ "data-list"; "active"; "hide-empty"; "relative" ] ]
      %> CCList.return
    in
    match results with
    | None -> []
    | Some [] -> [ txt "No results found" ] |> wrap
    | Some results -> results |> CCList.map item_to_html |> wrap
  in
  let attrs =
    [ a_input_type `Search
    ; a_value value
    ; a_name Field.(show query_field)
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
  |> div ~a:[ a_class [ "relative" ]; a_user_data "query" "input" ]
;;

let create
  item_to_html
  placeholder
  field_label
  ?(current = [])
  ?disabled
  ?hint
  ?role
  ?exclude_roles_of
  ?value
  ?results
  language
  path
  =
  let search_role = hidden_input Field.Role Role.Role.show role in
  let exclude_roles_of =
    hidden_input Field.ExcludeRolesOf Admin.(Id.value) exclude_roles_of
  in
  div
    ~a:[ a_class [ "form-group" ]; a_user_data "query" "wrapper" ]
    ([ label [ txt Pool_common.(Utils.nav_link_to_string language field_label) ]
     ; input_element item_to_html placeholder ?disabled ?value ?results path
     ; div
         ~a:
           [ a_user_data "query" "results"
           ; a_user_data "search-selection" ""
           ; a_class [ "hide-empty" ]
           ]
         (CCList.map item_to_html current)
     ]
     @ CCOption.map_or
         ~default:[]
         (fun hint ->
           [ span
               ~a:[ a_class [ "help" ] ]
               [ Pool_common.(Utils.hint_to_string language hint) |> txt ]
           ])
         hint
     @ search_role
     @ exclude_roles_of)
;;

let search_item ~id ~title =
  div
    ~a:[ a_user_data "id" id; a_user_data "selection-item" "" ]
    [ Component_icon.(to_html ~classnames:[ "toggle-item" ] Close)
    ; span [ txt title ]
    ; input
        ~a:
          [ a_input_type `Checkbox
          ; a_class [ "hidden" ]
          ; a_name Field.(array_key Value)
          ; a_value id
          ; a_checked ()
          ]
        ()
    ]
;;

module Experiment = struct
  let item (id, title) =
    let open Experiment in
    search_item ~id:(Id.value id) ~title:(Title.value title)
  ;;

  let placeholder = "Search by experiment title"
  let input_element = input_element item placeholder
  let create = create item placeholder I18n.Experiments
end

module Location = struct
  let item (id, name) =
    let open Pool_location in
    search_item ~id:(Id.value id) ~title:(Name.value name)
  ;;

  let placeholder = "Search by location name"
  let input_element = input_element item placeholder
  let create = create item placeholder I18n.Locations
end

module Tag = struct
  let item (id, title) =
    let open Tags in
    search_item ~id:(Id.value id) ~title:(Title.value title)
  ;;

  let placeholder = "Search by tag title"
  let input_element = input_element item placeholder
  let create = create item placeholder I18n.Tags
end
