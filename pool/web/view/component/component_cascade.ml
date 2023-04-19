open CCFun
open Tyxml.Html
module Input = Component_input
module Icon = Component_icon

let print = Utils.ppx_printer

module Utils = Component_utils

let notification_id = "cascade-notification"

module Roles = struct
  let action_path admin =
    Format.asprintf "/admin/admins/%s/%s" Admin.(admin |> id |> Id.value)
  ;;

  let value_input ?exclude_roles_of ?role ?value language =
    let open Role.Actor in
    function
    | Some QueryLocations ->
      Component_search.Location.create
        ?exclude_roles_of
        ?role
        ?value
        language
        "/admin/locations/search"
    | Some QueryExperiments ->
      Component_search.Experiment.create
        ?exclude_roles_of
        ?role
        language
        "/admin/experiments/search"
    | None -> div []
  ;;

  let value_form language ?exclude_roles_of ?key ?value () =
    CCOption.map_or ~default:(Ok None) Role.Actor.type_of_key key
    |> function
    | Error err -> p [ Pool_common.Utils.error_to_string language err |> txt ]
    | Ok input_type ->
      let input_field =
        value_input ?exclude_roles_of ?role:key ?value language input_type
      in
      div ~a:[ a_class [ "switcher-sm"; "flex-gap" ] ] [ input_field ]
  ;;

  let role_form ?key ?value language admin identifier role_list =
    let toggle_id = Format.asprintf "cascade-%i" identifier in
    let toggled_content = value_form language ?key ?value () in
    let key_selector =
      let attributes =
        Utils.htmx_attribs
          ~action:(action_path admin "toggle-role")
          ~trigger:"change"
          ~swap:"innerHTML"
          ~target:(Utils.as_target_id toggle_id)
          ~allow_empty_values:true
          ()
      in
      Component_input.selector
        ~attributes
        ~add_empty:true
        ~option_formatter:Role.Actor.key_to_string
        ~required:true
        ~classnames:[ "key-select" ]
        language
        Pool_common.Message.Field.Role
        Role.Actor.show
        role_list
        key
        ()
    in
    let submit_btn =
      let action = action_path admin "grant-role" in
      Component_input.submit_element
        language
        ~classnames:[ "push"; "align-self-end" ]
        ~attributes:
          (a_id "submit-cascade-form"
           :: Utils.htmx_attribs ~action ~swap:"none" ~trigger:"click" ())
        Pool_common.Message.(Add None)
        ()
    in
    div
      ~a:[ a_class [ "flexrow"; "flex-gap" ] ]
      [ key_selector
      ; div ~a:[ a_id toggle_id; a_class [ "grow-2" ] ] [ toggled_content ]
      ; submit_btn
      ]
  ;;

  let input_form ?(identifier = 0) ?key ?value csrf language admin role_list () =
    let role_form = role_form ?key ?value language admin identifier role_list in
    let stack = "stack-sm" in
    div
      ~a:[ a_class [ stack; "inset-sm"; "border"; "cascade" ] ]
      [ div
          ~a:[ a_id "cascade-form"; a_user_data "detect-unsaved-changes" "" ]
          [ div ~a:[ a_id notification_id ] []
          ; Component_input.csrf_element csrf ()
          ; div
              ~a:[ a_class [ stack; "grow"; "cascade-wrapper" ] ]
              [ role_form ]
          ]
      ]
  ;;
end
