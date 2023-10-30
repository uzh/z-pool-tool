open CCFun
open Tyxml.Html
module Field = Pool_common.Message.Field
module I18n = Pool_common.I18n

let query_field = Field.Search

type 'a dynamic_search =
  { hx_url : string
  ; hx_method : [ `Get | `Post ]
  ; to_label : 'a -> string
  ; to_value : 'a -> string
  ; selected : 'a list
  }

type 'a multi_search =
  | Dynamic of 'a dynamic_search
  | Static of 'a Component_input.multi_select

let default_query_results_item ~to_label ~to_value item =
  span
    ~a:[ a_class [ "data-item" ]; a_user_data "value" (to_value item) ]
    [ txt (to_label item) ]
;;

let query_results to_item items =
  div ~a:[ a_class [ "data-list"; "relative" ] ] (CCList.map to_item items)
;;

let multi_search
  langauge
  field
  multi_search
  ?(additional_attributes = [])
  ?(disabled = false)
  ?hint
  ?(is_filter = false)
  ?js_callback
  ?input_type
  ?placeholder
  ?tag_name
  ()
  =
  let open Pool_common in
  let filter_selected_attributes =
    match input_type with
    | None -> []
    | Some input_type ->
      [ a_user_data "input-type" (Filter.Key.show_input_type input_type) ]
  in
  let tag_name = CCOption.value ~default:field tag_name in
  let selected_item to_label to_value item =
    span
      ~a:[ a_user_data "selection-item" "" ]
      [ txt (to_label item)
      ; Component_icon.(to_html Close)
      ; input
          ~a:
            ([ a_input_type `Checkbox
             ; a_value (to_value item)
             ; a_name (Message.Field.array_key tag_name)
             ; a_checked ()
             ; a_hidden ()
             ]
             @ filter_selected_attributes)
          ()
      ]
  in
  let hint = Component_input.Elements.help langauge hint in
  let base_attributes =
    let placeholder =
      CCOption.map_or
        ~default:[]
        CCFun.(a_placeholder %> CCList.return)
        placeholder
    in
    let disabled = if disabled then [ a_disabled () ] else [] in
    let js_callback =
      match js_callback with
      | Some cb -> [ a_user_data "callback" cb ]
      | None -> []
    in
    [ a_input_type `Search
    ; a_name (Message.Field.show query_field)
    ; a_user_data "name" (Message.Field.array_key tag_name)
    ]
    @ placeholder
    @ disabled
    @ filter_selected_attributes
    @ js_callback
  in
  let wrap html =
    (* TODO: Place hint *)
    div
      ~a:[ a_class [ "form-group" ] ]
      ((label
          [ txt
              (Utils.field_to_string langauge field |> CCString.capitalize_ascii)
          ]
        :: html)
       @ hint)
  in
  let selected_items_wrapper items =
    div
      ~a:
        (a_user_data "search-selection" ""
         :: (if is_filter then [ a_user_data "query" "results" ] else []))
      items
  in
  match multi_search with
  | Static { Component_input.options; selected; to_label; to_value } ->
    [ input
        ~a:
          ((a_user_data "search" "static" :: base_attributes)
           @ additional_attributes)
        ()
    ; div
        ~a:[ a_class [ "data-list"; "relative" ] ]
        (CCList.map (default_query_results_item ~to_label ~to_value) options)
    ; selected_items_wrapper
        (CCList.map (selected_item to_label to_value) selected)
    ]
    |> wrap
  | Dynamic { hx_url; hx_method; to_label; to_value; selected } ->
    let url = Sihl.Web.externalize_path hx_url in
    let hx_method =
      match hx_method with
      | `Get -> a_user_data "hx-get" url
      | `Post -> a_user_data "hx-post" url
    in
    [ div
        ~a:[ a_class [ "relative"; "stack-xs" ] ]
        [ input
            ~a:
              ((base_attributes
                @ [ a_user_data "search" ""
                  ; hx_method
                  ; a_user_data "hx-trigger" "keyup changed delay:400ms"
                  ; a_user_data "hx-target" "next .data-list"
                  ])
               @ additional_attributes)
            ()
        ; div ~a:[ a_class [ "data-list"; "relative" ] ] []
        ; selected_items_wrapper
            (CCList.map (selected_item to_label to_value) selected)
        ]
    ]
    |> wrap
;;

let hidden_input name decode =
  CCOption.map_or ~default:[] (fun value ->
    [ input
        ~a:[ a_hidden (); a_name (Field.show name); a_value (decode value) ]
        ()
    ])
;;

module Experiment = struct
  open Experiment

  let placeholder = "Search by experiment title"
  let to_label = snd %> Title.value
  let to_value = fst %> Id.value

  let create ?disabled ?hint ?is_filter ?tag_name ?(selected = []) language =
    let dynamic_search =
      { hx_url = "/admin/experiments/search"
      ; hx_method = `Post
      ; to_label
      ; to_value
      ; selected
      }
    in
    multi_search
      ?disabled
      ?hint
      ?is_filter
      ~placeholder
      ?tag_name
      language
      Field.Experiments
      (Dynamic dynamic_search)
  ;;

  let filter_multi_search ~selected ~disabled language =
    create
      ~selected
      ~disabled
      ~is_filter:true
      ~tag_name:Pool_common.Message.Field.Value
      language
  ;;

  let assign_contact_search language contact =
    let dynamic_search =
      { hx_url =
          Format.asprintf
            "/admin/contacts/%s/experiments"
            (Contact.id contact |> Pool_common.Id.value)
      ; hx_method = `Get
      ; to_label
      ; to_value
      ; selected = []
      }
    in
    multi_search
      ~placeholder
      ~js_callback:"assign-contact"
      language
      Field.Experiments
      (Dynamic dynamic_search)
  ;;

  let query_results =
    CCList.map (default_query_results_item ~to_label ~to_value)
  ;;
end

module Location = struct
  open Pool_location

  let placeholder = "Search by location name"
  let to_label ({ name; _ } : t) = Name.value name
  let to_value { id; _ } = Id.value id

  let create ?disabled ?hint ?is_filter ?tag_name ?(selected = []) language =
    let dynamic_search =
      ({ hx_url = "/admin/locations/search"
       ; hx_method = `Get
       ; to_label
       ; to_value
       ; selected
       }
       : t dynamic_search)
    in
    multi_search
      ?disabled
      ?hint
      ?is_filter
      ~placeholder
      ?tag_name
      language
      Field.Locations
      (Dynamic dynamic_search)
  ;;

  let query_results =
    let open CCFun in
    let to_label = snd %> Name.value in
    let to_value = fst %> Id.value in
    CCList.map (default_query_results_item ~to_label ~to_value)
  ;;
end

module Tag = struct
  open Tags

  let placeholder = "Search by tag title"
  let to_label = snd %> Title.value
  let to_value = fst %> Id.value

  let create ?disabled ?hint ?is_filter ?tag_name ?(selected = []) language =
    let dynamic_search =
      { hx_url = "/admin/settings/tags/search"
      ; hx_method = `Post
      ; to_label
      ; to_value
      ; selected
      }
    in
    multi_search
      ?disabled
      ?hint
      ?is_filter
      ~placeholder
      ?tag_name
      language
      Field.Tag
      (Dynamic dynamic_search)
  ;;

  let filter_multi_search ~selected ~disabled language =
    create
      ~selected
      ~disabled
      ~is_filter:true
      ~tag_name:Pool_common.Message.Field.Value
      language
  ;;

  let query_results =
    CCList.map (default_query_results_item ~to_label ~to_value)
  ;;
end
