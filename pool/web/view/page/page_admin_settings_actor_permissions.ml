open CCFun
open Containers
open Tyxml.Html
module HttpUtils = Http_utils
module Input = Component.Input

let actor_permission_path ?suffix () =
  let default = "/admin/settings/actor-permission/" in
  CCOption.map_or ~default (Format.asprintf "%s%s" default) suffix
;;

let list Pool_context.{ language; csrf; guardian; _ } actor_permissions query =
  let open Pool_common in
  let can_do permission =
    let open Guard in
    PermissionOnTarget.(validate (create permission `Permission)) guardian
  in
  let can_create = can_do Guard.Permission.Create in
  let can_delete = can_do Guard.Permission.Delete in
  let url = Uri.of_string (actor_permission_path ()) in
  let data_table = Component.DataTable.create_meta url query language in
  let create_permission : [ | Html_types.flow5 ] elt =
    Input.link_as_button
      ~style:`Success
      ~icon:Component.Icon.Add
      ~control:(language, Pool_message.(Control.Add (Some Field.Permission)))
      (actor_permission_path ~suffix:"new" ())
  in
  let cols =
    Guard.ActorPermission.
      [ `column column_actor
      ; `column column_action
      ; `column column_target
      ; (if can_create then `custom create_permission else `empty)
      ]
  in
  let th_class = [ "w-3"; "w-3"; "w-3"; "w-3" ] in
  let row
        (( ({ Guard.ActorPermission.actor_uuid; permission; target } as actor_permission)
         , actor_name
         , target_model
         , target_name ) :
          Guard.ActorPermission.t * string * Role.Target.t option * string option)
    =
    let open Component.Role in
    let button_form target name submit_type confirm_text actor_permission =
      form
        ~a:
          [ a_method `Post
          ; a_action (actor_permission_path ~suffix:target () |> Sihl.Web.externalize_path)
          ; a_user_data
              "confirmable"
              (Pool_common.Utils.confirmable_to_string language confirm_text)
          ]
        [ Input.csrf_element csrf ()
        ; Input.input_element
            ~value:
              (actor_permission
               |> Guard.ActorPermission.to_yojson
               |> Yojson.Safe.to_string)
            language
            `Hidden
            Input.Field.Permission
        ; Input.submit_element ~submit_type language (name None) ()
        ]
    in
    let create_link text =
      let default = txt text in
      CCOption.map_or ~default (fun uri ->
        a ~a:[ a_href (Sihl.Web.externalize_path uri) ] [ default ])
    in
    let actor_link =
      create_target_path
        ~uuid:Guard.Uuid.(target_of Actor.to_string actor_uuid)
        (Some `Admin)
      |> create_link actor_name
    in
    let target_link =
      match target with
      | Guard.TargetEntity.Id uuid ->
        let name =
          let open CCOption in
          choice [ target_name; CCOption.map Role.Target.show target_model ]
          |> value ~default:"all"
        in
        create_target_path ~uuid target_model |> create_link name
      | Guard.TargetEntity.Model model ->
        create_target_path (Some model)
        |> create_link (CCOption.value ~default:(Role.Target.show model) target_name)
    in
    [ actor_link
    ; txt (Guard.Permission.show permission)
    ; target_link
    ; (if can_delete
       then
         div
           ~a:[ a_class [ "flexrow"; "flex-gap"; "justify-end" ] ]
           [ button_form
               "remove"
               Pool_message.Control.delete
               `Error
               I18n.RemovePermission
               actor_permission
           ]
       else txt "")
    ]
    |> CCList.map (CCList.return %> td)
    |> tr
  in
  Component.DataTable.make
    ~target_id:"permissions-table"
    ~th_class
    ~cols
    ~row
    data_table
    actor_permissions
;;

let read_hint =
  CCOption.map_or ~default:[] (I18n.content_to_string %> Unsafe.data %> CCList.return)
;;

let index ?hint (Pool_context.{ language; _ } as context) rules query =
  let open Pool_common in
  [ h1
      ~a:[ a_class [ "heading-1" ] ]
      [ txt (Utils.nav_link_to_string language I18n.ActorPermissions) ]
  ]
  @ read_hint hint
  @ [ list context rules query ]
  |> div ~a:[ a_class [ "trim"; "safety-margin" ] ]
;;

let create ?hint Pool_context.{ language; _ } children =
  let open Pool_common in
  [ h1
      ~a:[ a_class [ "heading-1" ] ]
      [ txt
          (Utils.control_to_string
             language
             Pool_message.(Control.Create (Some Field.Permission)))
      ]
  ]
  @ read_hint hint
  @ children
  |> div ~a:[ a_class [ "trim"; "safety-margin" ] ]
;;
