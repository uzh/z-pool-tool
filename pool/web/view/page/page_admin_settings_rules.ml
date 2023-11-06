open CCFun
open Tyxml.Html
module HttpUtils = Http_utils
module Input = Component.Input

let role_permission_path ?suffix () =
  let default = "/admin/settings/role-permission/" in
  CCOption.map_or ~default (Format.asprintf "%s%s" default) suffix
  |> Sihl.Web.externalize_path
;;

module List = struct
  let row
    Pool_context.{ csrf; language; _ }
    can_manage
    ({ Guard.RolePermission.role; permission; model } as role_permission)
    =
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
    let buttons rule =
      Pool_common.
        [ button_form "remove" Message.delete `Error I18n.RemoveRule rule ]
      |> div ~a:[ a_class [ "flexrow"; "flex-gap"; "justify-end" ] ]
    in
    [ txt ([%show: Role.Role.t] role)
    ; txt ([%show: Guard.Permission.t] permission)
    ; txt ([%show: Role.Target.t] model)
    ]
    @ if can_manage then [ buttons role_permission ] else []
  ;;

  let create ({ Pool_context.language; guardian; _ } as context) rules =
    let open Pool_common in
    let can_manage =
      Guard.PermissionOnTarget.validate
        Guard.(PermissionOnTarget.create Permission.Manage `Permission)
        guardian
    in
    let thead =
      (Message.Field.[ Role; Action; Model ]
       |> Component.Table.fields_to_txt language)
      @ if can_manage then [ txt "" ] else []
    in
    CCList.map (row context can_manage) rules
    |> Component.Table.horizontal_table `Striped ~thead
  ;;
end

let index ({ Pool_context.language; _ } as context) rules =
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
    ; List.create context rules
    ]
;;
