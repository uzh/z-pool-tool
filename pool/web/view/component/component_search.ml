open CCFun
open Tyxml.Html
module Field = Pool_message.Field
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

let with_empty_message language = function
  | [] ->
    [ span
        ~a:[ a_class [ "data-item" ] ]
        [ txt Pool_common.(Utils.text_to_string language I18n.EmptyListGeneric)
        ]
    ]
  | html -> html
;;

let query_results to_item items =
  div ~a:[ a_class [ "data-list"; "relative" ] ] (CCList.map to_item items)
;;

let multi_search
  ?(query_field = query_field)
  language
  field
  multi_search
  ?(additional_attributes = [])
  ?(disabled = false)
  ?hints
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
             ; a_name (Pool_message.Field.array_key tag_name)
             ; a_checked ()
             ; a_hidden ()
             ]
             @ filter_selected_attributes)
          ()
      ]
  in
  let hint = Component_input.Elements.hints language hints in
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
    ; a_name (Pool_message.Field.show query_field)
    ; a_user_data "name" (Pool_message.Field.array_key tag_name)
    ]
    @ placeholder
    @ disabled
    @ filter_selected_attributes
    @ js_callback
  in
  let wrap html =
    div
      ~a:
        (a_class [ "form-group" ]
         :: (if is_filter then [ a_user_data "query" "wrapper" ] else []))
      ((label
          [ txt
              (Utils.field_to_string language field |> CCString.capitalize_ascii)
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

let additional_filter_attributes =
  [ a_user_data
      "hx-params"
      Pool_message.Field.(
        [ array_key Value; show Search ] |> CCString.concat ", ")
  ]
;;

module Experiment = struct
  open Experiment

  let field = Field.Experiments
  let placeholder = "Search by experiment title"
  let to_label = snd %> Title.value
  let to_value = fst %> Id.value

  let dynamic_search ?(selected = []) hx_url hx_method =
    { hx_url; hx_method; to_label; to_value; selected }
  ;;

  let filter_multi_search ?selected ~disabled language =
    let dynamic_search =
      dynamic_search ?selected "/admin/experiments/search" `Post
    in
    multi_search
      ~disabled
      ~is_filter:true
      ~tag_name:Pool_message.Field.Value
      language
      field
      (Dynamic dynamic_search)
      ()
  ;;

  let assign_contact_search language contact =
    let dynamic_search =
      dynamic_search
        (Format.asprintf
           "/admin/contacts/%s/experiments"
           (Contact.id contact |> Pool_user.Id.value))
        `Get
    in
    multi_search
      ~placeholder
      ~js_callback:"assign-contact"
      language
      Field.Experiments
      (Dynamic dynamic_search)
  ;;

  let query_results language items =
    CCList.map (default_query_results_item ~to_label ~to_value) items
    |> with_empty_message language
  ;;
end

module Location = struct
  open Pool_location

  let field = Field.Locations
  let placeholder = "Search by location name"
  let to_label ({ name; _ } : t) = Name.value name
  let to_value { id; _ } = Id.value id

  let dynamic_search ?(selected = []) hx_url hx_method : t dynamic_search =
    { hx_url; hx_method; to_label; to_value; selected }
  ;;

  let query_results language items =
    let open CCFun in
    let to_label = snd %> Name.value in
    let to_value = fst %> Id.value in
    CCList.map (default_query_results_item ~to_label ~to_value) items
    |> with_empty_message language
  ;;
end

module Tag = struct
  open Tags

  let placeholder = "Search by tag title"
  let to_label = snd %> Title.value
  let to_value = fst %> Id.value

  let create ?disabled ?hints ?is_filter ?tag_name ?(selected = []) language =
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
      ?hints
      ?is_filter
      ~placeholder
      ?tag_name
      language
      Field.Tag
      (Dynamic dynamic_search)
  ;;

  let filter_multi_search ~selected ~disabled language =
    create
      ~disabled
      ~is_filter:true
      ~selected
      ~tag_name:Pool_message.Field.Value
      language
  ;;

  let query_results language items =
    CCList.map (default_query_results_item ~to_label ~to_value) items
    |> with_empty_message language
  ;;
end

module RoleTarget = struct
  let hx_url admin_id =
    Format.asprintf "/admin/admins/%s/search-role" Pool_user.(Id.value admin_id)
  ;;

  let additional_attributes =
    [ a_user_data
        "hx-params"
        Pool_message.Field.(
          [ array_key Target; show Role; show Search ] |> CCString.concat ", ")
    ]
  ;;

  let experiments ?hints language admin_id =
    let open Experiment in
    multi_search
      ~additional_attributes
      ?hints
      ~tag_name:Field.Target
      ~placeholder
      language
      field
      (Dynamic (dynamic_search (hx_url admin_id) `Post))
      ()
  ;;

  let locations ?hints language admin_id =
    let open Location in
    multi_search
      ~additional_attributes
      ?hints
      ~tag_name:Field.Target
      ~placeholder
      language
      field
      (Dynamic (dynamic_search (hx_url admin_id) `Post))
      ()
  ;;
end

module Admin = struct
  let field = Field.Admin
  let placeholder = "Search by admin name or email"

  let to_label admin =
    Format.asprintf
      "%s (%a)"
      (admin |> Admin.user |> Pool_user.user_fullname)
      Pool_user.EmailAddress.pp
      (admin |> Admin.email_address)
  ;;

  let to_value = Admin.id %> Pool_user.Id.value

  let hx_url admin_id =
    Format.asprintf "/admin/admins/%s/search-role" Pool_user.(Id.value admin_id)
  ;;

  let dynamic_search ?(selected = []) hx_url hx_method =
    { hx_url; hx_method; to_label; to_value; selected }
  ;;

  let create ?selected ~disabled language =
    let dynamic_search =
      dynamic_search ?selected "/admin/admins/search" `Post
    in
    multi_search
      ~query_field:Field.(SearchOf Admin)
      ~disabled
      ~is_filter:true
      ~tag_name:Pool_message.Field.(ValueOf Admin)
      language
      field
      (Dynamic dynamic_search)
      ()
  ;;

  let query_results language items =
    CCList.map (default_query_results_item ~to_label ~to_value) items
    |> with_empty_message language
  ;;
end
