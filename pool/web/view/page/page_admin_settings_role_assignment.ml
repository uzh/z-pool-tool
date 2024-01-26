open CCFun
open Containers
open Tyxml.Html
module HttpUtils = Http_utils
module Input = Component.Input

let role_assignment_path ?suffix () =
  let default = "/admin/settings/role-assignment/" in
  CCOption.map_or ~default (Format.asprintf "%s%s" default) suffix
;;

let list Pool_context.{ language; csrf; guardian; _ } role_assignments query =
  let open Pool_common in
  let open Guard in
  let can_manage =
    PermissionOnTarget.validate
      (PermissionOnTarget.create Permission.Manage `RoleAssignment)
      guardian
  in
  let url = Uri.of_string (role_assignment_path ()) in
  let data_table = Component.DataTable.create_meta url query language in
  let cols =
    [ `column RoleAssignment.column_role
    ; `column RoleAssignment.column_target_role
    ; `empty
    ]
  in
  let th_class = [ "w-3"; "w-3"; "w-3"; "w-3" ] in
  let row (role_assignment : RoleAssignment.t) =
    let open RoleAssignment in
    let button_form target name submit_type confirm_text role_assignment =
      form
        ~a:
          [ a_method `Post
          ; a_action
              (role_assignment_path ~suffix:target ()
               |> Sihl.Web.externalize_path)
          ; a_user_data
              "confirmable"
              (Pool_common.Utils.confirmable_to_string language confirm_text)
          ]
        [ Input.csrf_element csrf ()
        ; Input.input_element
            ~value:
              (role_assignment
               |> RoleAssignment.to_yojson
               |> Yojson.Safe.to_string)
            language
            `Hidden
            Input.Field.Rule
        ; Input.submit_element ~submit_type language (name None) ()
        ]
    in
    [ txt (Role.Role.show role_assignment.role)
    ; txt (Role.Role.show role_assignment.target_role)
    ; (if can_manage
       then
         div
           ~a:[ a_class [ "flexrow"; "flex-gap"; "justify-end" ] ]
           [ button_form
               "remove"
               Message.delete
               `Error
               I18n.RemoveAssignableRole
               role_assignment
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
    role_assignments
;;

let index (Pool_context.{ language; _ } as context) rules query =
  div
    ~a:[ a_class [ "trim"; "safety-margin" ] ]
    [ h1
        ~a:[ a_class [ "heading-1" ] ]
        [ txt
            Pool_common.(Utils.nav_link_to_string language I18n.RolePermissions)
        ]
    ; p
        [ Pool_common.(Utils.hint_to_string language I18n.RolePermissionsIntro)
          |> HttpUtils.add_line_breaks
        ]
    ; list context rules query
    ]
;;
