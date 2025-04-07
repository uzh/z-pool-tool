open CCFun
open Tyxml.Html
open Pool_message
open Control
module HttpUtils = Http_utils
module Icon = Component.Icon
module Input = Component.Input
module Table = Component.Table

let role_permission_path = HttpUtils.Url.Admin.role_permission_path
let edit_permission_modal_id = "edit-permission-modal"

let edit_target_modal
      { Pool_context.language; csrf; _ }
      ?error
      ?flash_fetcher
      role
      target
      current_permissions
  =
  let open Role in
  let action =
    HttpUtils.Url.Admin.role_permission_target_path ~target role ()
    |> Sihl.Web.externalize_path
  in
  let all_permissions = Guard.Permission.all in
  let title language =
    let field = Field.PermissionOn (Role.show role, Target.name target) in
    Pool_common.(Utils.control_to_string language (Edit (Some field)))
    |> CCString.capitalize_ascii
  in
  let html =
    let checkbox permission =
      let name = Guard.Permission.show permission in
      let checked =
        (match flash_fetcher with
         | Some flash_fetcher ->
           flash_fetcher name |> CCOption.map_or ~default:false Utils.Bool.of_string
         | None -> CCList.mem ~eq:Guard.Permission.equal permission current_permissions)
        |> function
        | true -> [ a_checked () ]
        | false -> []
      in
      div
        ~a:[ a_class [ "form-group" ] ]
        [ div
            [ input ~a:([ a_input_type `Checkbox; a_id name; a_name name ] @ checked) ()
            ; label ~a:[ a_label_for name ] [ txt (CCString.capitalize_ascii name) ]
            ]
        ]
    in
    let error =
      error
      |> CCOption.map_or ~default:(txt "") (fun err ->
        [ txt (Pool_common.Utils.error_to_string language err) ]
        |> Component.Notification.notification language `Error)
    in
    let permissions = all_permissions |> CCList.map checkbox in
    div
      ~a:[ a_class [ "stack" ] ]
      [ p Pool_common.[ Utils.hint_to_string language I18n.Permissions |> Unsafe.data ]
      ; error
      ; form
          ~a:
            Htmx.
              [ hx_trigger "submit"
              ; hx_post action
              ; hx_swap "outerHTML"
              ; hx_target (Format.asprintf "#%s" edit_permission_modal_id)
              ; a_class [ "flexcolumn"; "stack" ]
              ]
          ((Input.csrf_element csrf () :: permissions)
           @ [ div
                 ~a:[ a_class [ "flexrow"; "justify-end" ] ]
                 [ Input.submit_element
                     ~has_icon:Icon.Save
                     language
                     (Save (Some Field.Permission))
                     ()
                 ]
             ])
      ]
  in
  Component.Modal.create ~active:true language title edit_permission_modal_id html
;;

let list
      Pool_context.{ language; guardian; _ }
      role
      (rules : (Guard.Persistence.target_model * Guard.Permission.t list) list)
      query
  =
  let open Guard in
  let can_manage =
    PermissionOnTarget.validate
      (PermissionOnTarget.create Permission.Manage `Permission)
      guardian
  in
  let query = Query.{ query with pagination = None } in
  let url = Uri.of_string (role_permission_path ~role ()) in
  let data_table = Component.DataTable.create_meta url query language in
  let cols =
    [ `column RolePermission.column_model
    ; `custom
        (txt Pool_common.(Utils.field_to_string_capitalized language Field.Permission))
    ; `empty
    ]
  in
  let th_class = [ "w-3"; "w-3"; "w-3"; "w-3" ] in
  let row (target, permissions) =
    let edit_button () =
      Input.link_as_button
        "#"
        ~attributes:
          Htmx.
            [ hx_trigger "click"
            ; hx_get
                (HttpUtils.Url.Admin.role_permission_target_path
                   ~suffix:"edit"
                   ~target
                   role
                   ())
            ; hx_swap "outerHTML"
            ; hx_target (Format.asprintf "#%s" edit_permission_modal_id)
            ]
        ~icon:Component.Icon.Create
    in
    [ txt (Role.Target.show target)
    ; permissions
      |> CCList.map (Permission.show %> Component.Tag.create_chip ~inline:true `Primary)
      |> div ~a:[ a_class [ "flexrow"; "flex-gap" ] ]
    ; (if can_manage then edit_button () else txt "")
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
    rules
;;

let show (Pool_context.{ language; _ } as context) role rules query =
  div
    ~a:[ a_class [ "trim"; "safety-margin" ] ]
    [ h1
        ~a:[ a_class [ "heading-1"; "has-gap" ] ]
        [ txt (Role.Role.name role |> CCString.capitalize_ascii) ]
    ; p [ txt Pool_common.(Utils.hint_to_string language I18n.RolePermissionsModelList) ]
    ; list context role rules query
    ; Component.Modal.create_placeholder edit_permission_modal_id
    ]
;;

let index Pool_context.{ language; _ } roles =
  let open Role.Role in
  let table =
    let thead = (Field.[ Role ] |> Table.fields_to_txt language) @ [ txt "" ] in
    let row role =
      [ txt (name role |> CCString.capitalize_ascii)
      ; Input.link_as_button ~icon:Icon.Eye (role_permission_path ~role ())
      ]
    in
    roles |> CCList.map row |> Table.horizontal_table ~align_last_end:true ~thead `Striped
  in
  div
    ~a:[ a_class [ "trim"; "safety-margin" ] ]
    [ h1
        ~a:[ a_class [ "heading-1"; "has-gap" ] ]
        [ txt Pool_common.(Utils.nav_link_to_string language I18n.RolePermissions) ]
    ; div ~a:[ a_class [ "stack" ] ] [ Component.Role.explanation_modal language; table ]
    ]
;;
