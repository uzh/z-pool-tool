open CCFun
open Tyxml.Html
module Input = Component_input
module Table = Component_table
module Icon = Component_icon
module Field = Pool_common.Message.Field

let print = Utils.ppx_printer

module Utils = Component_utils

let roles_path ?suffix admin =
  let default =
    Format.asprintf "/admin/admins/%s/" Admin.(id admin |> Id.value)
  in
  CCOption.map_or ~default (Format.asprintf "%s%s" default) suffix
;;

let target_path ({ Guard.ActorRole.target_uuid; _ }, target_model) =
  let build path =
    Guard.Uuid.Target.to_string %> Format.asprintf "/admin/%s/%s/" path
  in
  CCOption.map2
    (fun uuid -> function
      | `Experiment -> Some (build "experiments" uuid)
      | `Location -> Some (build "locations" uuid)
      | `Admin -> Some (build "admins" uuid)
      | `Contact -> Some (build "contacts" uuid)
      | `CustomField -> Some (build "custom-fields/contact/field" uuid)
      | `CustomFieldGroup -> Some (build "custom-fields/contact/group" uuid)
      | `Filter -> Some (build "filter" uuid)
      | `Tag -> Some (build "settings/tags" uuid)
      | `Assignment
      | `ContactInfo
      | `ContactName
      | `I18n
      | `Invitation
      | `LocationFile
      | `Mailing
      | `Message
      | `MessageTemplate
      | `OrganisationalUnit
      | `Permission
      | `Queue
      | `Role
      | `Schedule
      | `Session
      | `SessionClose
      | `SystemSetting
      | `Smtp
      | `Statistics
      | `System
      | `Tenant
      | `WaitingList -> None)
    target_uuid
    target_model
  |> CCOption.flatten
;;

module List = struct
  let row
    ?(is_edit = false)
    Pool_context.{ csrf; language; _ }
    target_admin
    (( ({ Guard.ActorRole.actor_uuid; role; target_uuid } as actor_role)
     , (_ : Role.Target.t option) ) as role_element)
    =
    let button_form target name submit_type confirm_text =
      form
        ~a:
          [ a_method `Post
          ; a_action (roles_path ~suffix:target target_admin)
          ; a_user_data
              "confirmable"
              (Pool_common.Utils.confirmable_to_string language confirm_text)
          ]
        ([ Input.csrf_element csrf ()
         ; Input.submit_element ~submit_type language (name None) ()
         ; input
             ~a:
               [ a_name Input.Field.(show Role)
               ; a_value ([%show: Role.Role.t] role)
               ; a_hidden ()
               ]
             ()
         ; input
             ~a:
               [ a_name Input.Field.(show Actor)
               ; a_value ([%show: Guard.Uuid.Actor.t] actor_uuid)
               ; a_hidden ()
               ]
             ()
         ]
         @ CCOption.map_or
             ~default:[]
             (fun uuid ->
               [ input
                   ~a:
                     [ a_name Input.Field.(show Target)
                     ; a_value ([%show: Guard.Uuid.Target.t] uuid)
                     ; a_hidden ()
                     ]
                   ()
               ])
             target_uuid)
    in
    let buttons =
      let open Pool_common in
      let default = [] in
      let target_button =
        CCOption.map_or
          ~default
          (Input.link_as_button
             ~control:(language, Pool_common.Message.show)
             ~icon:Icon.Eye
           %> CCList.return)
          (target_path role_element)
      in
      let remove_button =
        if is_edit
        then [ button_form "revoke-role" Message.delete `Error I18n.RevokeRole ]
        else default
      in
      target_button @ remove_button
      |> div ~a:[ a_class [ "flexrow"; "flex-gap"; "justify-end" ] ]
    in
    [ txt (Guard.ActorRole.role_to_human actor_role); buttons ]
  ;;

  let create
    ?is_edit
    ({ Pool_context.language; _ } as context)
    target_admin
    roles
    =
    let open CCList in
    let thead =
      let open Pool_common.Message in
      (Field.[ Role ] |> Table.fields_to_txt language) @ [ txt "" ]
    in
    roles
    >|= row ?is_edit context target_admin
    |> Table.horizontal_table `Striped ~thead
  ;;
end

module Search = struct
  let action_path admin =
    Format.asprintf "/admin/admins/%s/%s" Admin.(admin |> id |> Id.value)
  ;;

  let value_input ?exclude_roles_of ?role ?value language =
    let open Role.Role in
    function
    | Some QueryLocations ->
      let hint = Pool_common.I18n.RoleIntro (Field.Location, Field.Locations) in
      Component_search.Location.create
        ?exclude_roles_of
        ~hint
        ?role
        ?value
        language
        "/admin/locations/search"
    | Some QueryExperiments ->
      let hint =
        Pool_common.I18n.RoleIntro (Field.Experiment, Field.Experiments)
      in
      Component_search.Experiment.create
        ?exclude_roles_of
        ~hint
        ?role
        language
        "/admin/experiments/search"
    | None -> div []
  ;;

  let value_form language ?exclude_roles_of ?key ?value () =
    CCOption.map_or ~default:(Ok None) Role.Role.type_of_key key
    |> function
    | Error err -> p [ Pool_common.Utils.error_to_string language err |> txt ]
    | Ok input_type ->
      let input_field =
        value_input ?exclude_roles_of ?role:key ?value language input_type
      in
      div ~a:[ a_class [ "switcher-sm"; "flex-gap" ] ] [ input_field ]
  ;;

  let role_form ?key ?value language admin identifier role_list =
    let toggle_id = Format.asprintf "role-search-%i" identifier in
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
        ~option_formatter:Role.Role.show
        ~required:true
        ~classnames:[ "key-select" ]
        language
        Field.Role
        Role.Role.show
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
          (a_id "submit-role-search-form"
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
      ~a:[ a_class [ stack; "inset-sm"; "border"; "role-search" ] ]
      [ div
          ~a:
            [ a_id "role-search-form"; a_user_data "detect-unsaved-changes" "" ]
          [ Component_input.csrf_element csrf ()
          ; div
              ~a:[ a_class [ stack; "grow"; "role-search-wrapper" ] ]
              [ role_form ]
          ]
      ]
  ;;
end
