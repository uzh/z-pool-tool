open Containers
open Tyxml.Html
module HttpUtils = Http_utils
module Input = Component.Input

let role_permission_path ?suffix () =
  let default = "/admin/settings/role-permission/" in
  CCOption.map_or ~default (Format.asprintf "%s%s" default) suffix
  |> Sihl.Web.externalize_path
;;

let index Pool_context.{ language; csrf; guardian; _ } rules query =
  let open Pool_common in
  let can_manage =
    Guard.PermissionOnTarget.validate
      Guard.(PermissionOnTarget.create Permission.Manage `Permission)
      guardian
  in
  let url = Uri.of_string (role_permission_path ()) in
  let sort = Component.Sortable_table.{ url; query; language } in
  let cols =
    [ `column Guard.column_role
    ; `column Guard.column_action
    ; `column Guard.column_model
    ; `empty
    ]
  in
  let rows =
    let button_form target name submit_type confirm_text role_permission =
      form
        ~a:
          [ a_method `Post
          ; a_action (role_permission_path ~suffix:target ())
          ; a_user_data
              "confirmable"
              (Pool_common.Utils.confirmable_to_string language confirm_text)
          ]
        [ Input.csrf_element csrf ()
        ; Input.input_element
            ~value:
              (role_permission
               |> Guard.RolePermission.to_yojson
               |> Yojson.Safe.to_string)
            language
            `Hidden
            Input.Field.Rule
        ; Input.submit_element ~submit_type language (name None) ()
        ]
    in
    let open Guard.RolePermission in
    let row (rule : Guard.RolePermission.t) =
      [ txt (Role.Role.show rule.role)
      ; txt (Guard.Permission.show rule.permission)
      ; txt (Role.Target.show rule.model)
      ; (if can_manage
         then
           div
             ~a:[ a_class [ "flexrow"; "flex-gap"; "justify-end" ] ]
             [ button_form "remove" Message.delete `Error I18n.RemoveRule rule ]
         else txt "")
      ]
    in
    List.map row rules
  in
  let target_id = "permissions-table" in
  div
    ~a:[ a_id target_id; a_class [ "trim"; "safety-margin" ] ]
    [ h1
        ~a:[ a_class [ "heading-1" ] ]
        [ txt
            Pool_common.(Utils.nav_link_to_string language I18n.RolePermissions)
        ]
    ; p
        [ Pool_common.(Utils.hint_to_string language I18n.RolePermissionsIntro)
          |> HttpUtils.add_line_breaks
        ]
    ; Component.Sortable_table.make ~target_id ~cols ~rows sort
    ]
;;
