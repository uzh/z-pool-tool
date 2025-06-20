open CCFun
open Tyxml.Html
module Input = Component_input
module Table = Component_table
module Icon = Component_icon
module Field = Pool_message.Field

let print = Utils.ppx_printer

module Utils = Component_utils

let roles_path base_path ?suffix target_id =
  let default =
    Format.asprintf "%s/%s/" base_path (Guard.Uuid.Target.to_string target_id)
  in
  CCOption.map_or ~default (Format.asprintf "%s%s" default) suffix
;;

let create_target_path ?uuid =
  let build path =
    CCOption.map_or ~default:"" Guard.Uuid.Target.to_string
    %> Format.asprintf "/admin/%s/%s/" path
  in
  flip CCOption.bind (function
    | `Admin -> Some (build "admins" uuid)
    | `Contact -> Some (build "contacts" uuid)
    | `CustomField -> Some (build "custom-fields/contact/field" uuid)
    | `CustomFieldGroup -> Some (build "custom-fields/contact/group" uuid)
    | `Experiment -> Some (build "experiments" uuid)
    | `Filter -> Some (build "filter" uuid)
    | `Location -> Some (build "locations" uuid)
    | `Tag -> Some (build "settings/tags" uuid)
    | `Announcement
    | `Assignment
    | `ApiKey
    | `ContactInfo
    | `ContactDirectMessage
    | `ContactName
    | `DuplicateContact
    | `I18n
    | `Invitation
    | `InvitationNotification
    | `LocationFile
    | `Mailing
    | `Message
    | `MessageTemplate
    | `OrganisationalUnit
    | `Permission
    | `Queue
    | `Role
    | `RoleAdmin
    | `RoleAssistant
    | `RoleExperimenter
    | `RoleLocationManager
    | `RoleOperator
    | `RoleRecruiter
    | `Schedule
    | `Session
    | `SessionClose
    | `SignupCode
    | `Smtp
    | `Statistics
    | `System
    | `SystemSetting
    | `Tenant
    | `Version
    | `WaitingList -> None)
;;

let roles_section ?(top_element = []) language children =
  let open Pool_common in
  div
    [ h2
        ~a:[ a_class [ "heading-2"; "has-gap" ] ]
        [ Utils.text_to_string language I18n.RolesGranted |> txt ]
    ; div ~a:[ a_class [ "stack" ] ] (top_element @ children)
    ]
;;

module List = struct
  let row
        ?(is_edit = false)
        ~path
        Pool_context.{ csrf; language; _ }
        target_id
        (( ({ Guard.ActorRole.actor_uuid; role; target_uuid } as actor_role)
         , (_ : Role.Target.t option)
         , (title : string option) ) as role_element)
    =
    let target_path ({ Guard.ActorRole.target_uuid; _ }, target_model, _) =
      create_target_path ?uuid:target_uuid target_model
    in
    let button_form target name submit_type confirm_text =
      form
        ~a:
          [ a_method `Post
          ; a_action
              (roles_path ~suffix:target path target_id |> Sihl.Web.externalize_path)
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
             ~control:(language, Pool_message.Control.Show)
             ~icon:Icon.Eye
           %> CCList.return)
          (target_path role_element)
      in
      let remove_button =
        if is_edit
        then
          [ button_form "revoke-role" Pool_message.Control.delete `Error I18n.RevokeRole ]
        else default
      in
      target_button @ remove_button
      |> div ~a:[ a_class [ "flexrow"; "flex-gap"; "justify-end" ] ]
    in
    let to_human { Guard.ActorRole.role; _ } =
      let role_human = Role.Role.show role in
      CCOption.map_or ~default:(txt role_human) (fun title ->
        div [ Format.asprintf "%s of " role_human |> txt; em [ txt title ] ])
    in
    to_human actor_role title :: [ buttons ]
  ;;

  let create ?is_edit ~path ({ Pool_context.language; _ } as context) target_id roles =
    let open CCList in
    let thead =
      let open Pool_message in
      (Field.[ Role ] |> Table.fields_to_txt language) @ [ txt "" ]
    in
    roles
    >|= row ?is_edit ~path context target_id
    |> Table.horizontal_table `Striped ~thead
  ;;
end

module Search = struct
  let action_path base_path target_id =
    Format.asprintf "%s/%s/%s" base_path (Guard.Uuid.Target.to_string target_id)
  ;;

  let value_input ~path language target_id =
    let open Role.Role in
    let open Pool_common.I18n in
    function
    | Some QueryLocations ->
      let hints = [ RoleIntro (Field.Location, Field.Locations) ] in
      Component_search.RoleTarget.locations ~hints ~path language target_id
    | Some QueryExperiments ->
      let hints = [ RoleIntro (Field.Experiment, Field.Experiments) ] in
      Component_search.RoleTarget.experiments ~hints ~path language target_id
    | None -> div []
  ;;

  let value_form ~path language target_id ?key () =
    CCOption.map_or ~default:(Ok None) Role.Role.type_of_key key
    |> function
    | Error err -> p [ Pool_common.Utils.error_to_string language err |> txt ]
    | Ok input_type ->
      let input_field = value_input ~path language target_id input_type in
      div ~a:[ a_class [ "switcher-sm"; "flex-gap" ] ] [ input_field ]
  ;;

  let role_form ?key ~path language csrf target_id role_list =
    let toggle_id = "role-search" in
    let toggled_content = value_form ~path language target_id ?key () in
    let key_selector =
      let attributes =
        Utils.htmx_attribs
          ~action:(action_path path target_id "toggle-role")
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
      Component_input.submit_element
        language
        ~classnames:[ "push"; "align-self-end" ]
        Pool_message.(Control.Add None)
        ()
    in
    form
      ~a:
        [ a_action (action_path path target_id "grant-role" |> Sihl.Web.externalize_path)
        ; a_method `Post
        ]
      [ Component_input.csrf_element csrf ()
      ; div
          ~a:[ a_class [ "flexrow"; "flex-gap" ] ]
          [ key_selector
          ; div ~a:[ a_id toggle_id; a_class [ "grow-2" ] ] [ toggled_content ]
          ; submit_btn
          ]
      ]
  ;;

  let input_form ?key ~path csrf language actor_id role_list () =
    let role_form = role_form ?key ~path language csrf actor_id role_list in
    let stack = "stack-sm" in
    div
      ~a:[ a_class [ stack; "inset-sm"; "border"; "role-search" ] ]
      [ div
          ~a:[ a_id "role-search-form"; a_user_data "detect-unsaved-changes" "" ]
          [ div ~a:[ a_class [ stack; "grow"; "role-search-wrapper" ] ] [ role_form ] ]
      ]
  ;;
end

module ActorPermissionSearch = struct
  let action_path = Format.asprintf "/admin/settings/actor-permission/%s"

  let value_form ?(empty = false) language =
    let open Component_search in
    function
    | `Admin when not empty -> Admin.create ~disabled:false language
    | `Experiment when not empty ->
      Experiment.filter_multi_search ~disabled:false language
    | _ -> div []
  ;;

  let actor_form ?flash_fetcher language csrf =
    let toggle_id = "actor-search" in
    let actor_form = value_form language `Admin in
    let attributes =
      Utils.htmx_attribs
        ~action:(action_path "toggle-target")
        ~trigger:"change"
        ~swap:"innerHTML"
        ~target:(Utils.as_target_id toggle_id)
        ~allow_empty_values:true
        ()
    in
    let init_permission = Some Guard.Permission.Create in
    let permission_selector =
      Component_input.selector
        ~classnames:[ "w-2" ]
        ~attributes
        ~required:true
        ?flash_fetcher
        language
        Field.Permission
        Guard.Permission.show
        Guard.Permission.[ Create; Read ]
        init_permission
        ()
    in
    let model_selector =
      Component_input.selector
        ~classnames:[ "w-2" ]
        ~attributes
        ~required:true
        ~read_only:true
        ?flash_fetcher
        language
        Field.Model
        Role.Target.show
        Role.Target.actor_permission
        (CCList.head_opt Role.Target.actor_permission)
        ()
    in
    let submit_btn =
      Component_input.submit_element
        language
        ~classnames:[ "push"; "align-self-end" ]
        Pool_message.(Control.Add None)
        ()
    in
    let value_form =
      value_form
        ~empty:
          (init_permission
           |> CCOption.map_or ~default:true Guard.Permission.(equal Create))
        language
        `Experiment
    in
    form
      ~a:
        [ a_action (action_path "" |> Sihl.Web.externalize_path)
        ; a_method `Post
        ; a_class [ "stack" ]
        ]
      [ Component_input.csrf_element csrf ()
      ; div
          ~a:[ a_class [ "flexrow"; "flex-gap" ] ]
          [ div ~a:[ a_class [ "w-4" ] ] [ actor_form ]
          ; permission_selector
          ; model_selector
          ; div ~a:[ a_id toggle_id; a_class [ "w-4" ] ] [ value_form ]
          ]
      ; div ~a:[ a_class [ "flexrow"; "flex-gap" ] ] [ submit_btn ]
      ]
  ;;

  let input_form ?flash_fetcher csrf language () =
    let create = actor_form ?flash_fetcher language csrf in
    div
      ~a:[ a_class [ "trim"; "actor-search" ] ]
      [ div
          ~a:[ a_id "actor-search-form"; a_user_data "detect-unsaved-changes" "" ]
          [ div ~a:[ a_class [ "grow"; "actor-search-wrapper" ] ] [ create ] ]
      ]
  ;;
end

let actor_explanation language =
  let open Pool_common in
  let to_html = function
    | Language.En ->
      "Actors are users that perform actions in the system. They can be assigned roles \
       to grant them permissions on targets."
    | Language.De ->
      "Actors sind Benutzer die Aktionen im System ausführen. Ihnen können Rollen \
       zugewiesen werden, um ihnen Berechtigungen auf Targets zu gewähren."
  in
  language |> to_html |> txt |> CCList.return |> div ~a:[ a_class [ "stack" ] ]
;;

let targets_explanation language =
  let open Pool_common in
  let to_html = function
    | Language.En ->
      "Targets are resources or entities that are accessed. A target can either be all \
       items (experiments) of a type or a specific item (experiment X)."
    | Language.De ->
      "Ein Target ist eine Ressource oder Entität, auf die zugegriffen wird. Ein Target \
       kann entweder alle Elemente (Experimente) eines Typs oder ein spezifisches \
       Element (Experiment X) sein."
  in
  language |> to_html |> txt |> CCList.return |> div ~a:[ a_class [ "stack" ] ]
;;

let permissions_explanation language =
  let open Pool_common in
  let open Guard.Permission in
  let title_html permission =
    h4 ~a:[ a_class [ "has-gap" ] ] [ txt (show permission |> CCString.capitalize_ascii) ]
  in
  let create = function
    | Language.En -> "Add new resources"
    | Language.De -> "Neue Ressourcen hinzufügen"
  in
  let read = function
    | Language.En -> "View existing resources"
    | Language.De -> "Vorhandene Ressourcen anzeigen"
  in
  let update = function
    | Language.En -> "Modify existing resources"
    | Language.De -> "Vorhandene Ressourcen modifizieren"
  in
  let delete = function
    | Language.En -> "Delete resources"
    | Language.De -> "Ressourcen löschen"
  in
  let manage = function
    | Language.En -> "Includes all permissions"
    | Language.De -> "Beinhaltet alle Berechtigungen"
  in
  let to_html = function
    | Create -> create
    | Read -> read
    | Update -> update
    | Delete -> delete
    | Manage -> manage
  in
  all
  |> CCList.map (fun role -> div [ title_html role; p [ txt (to_html role language) ] ])
  |> div ~a:[ a_class [ "stack" ] ]
;;

let explanation langauge =
  let open Pool_message in
  let title field =
    h3
      ~a:[ a_class [ "has-gap" ] ]
      [ txt (Pool_common.Utils.field_to_string_capitalized langauge field) ]
  in
  div
    [ title Field.Actor
    ; actor_explanation langauge
    ; title Field.Target
    ; targets_explanation langauge
    ; title Field.Permission
    ; permissions_explanation langauge
    ]
;;

let explanation_modal language =
  let open Pool_common in
  let title language =
    Pool_common.(Utils.nav_link_to_string language I18n.RolePermissions)
  in
  let content = explanation language in
  let link = Pool_common.Utils.hint_to_string language I18n.PermissionsExplanationLink in
  let modal =
    Component_modal.create ~active:false language title "explanation-modal" content
  in
  let link =
    a
      ~a:
        [ a_class [ "flexrow"; "flex-gap" ]
        ; a_user_data "modal" "explanation-modal"
        ; a_href "#"
        ]
      [ Component_icon.(to_html OpenOutline); txt link ]
  in
  div [ modal; link ]
;;
