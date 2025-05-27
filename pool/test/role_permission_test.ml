module Command = Cqrs_command.Guardian_command

module Data = struct
  open Guard.Permission

  let create = Create, true
  let read = Read, true
  let update = Update, true
  let delete = Delete, true
  let manage = Manage, true

  let to_urlencoded =
    CCList.map (fun (permission, value) ->
      show permission, [ Utils.Bool.to_string value ])
  ;;

  let handle_booleans = Http_utils.format_request_boolean_values (all |> CCList.map show)
end

let update_permissions () =
  let open CCResult in
  let role = `Assistant in
  let target = `SessionClose in
  let current_permissions =
    Guard.assistant_permissions
    |> CCList.filter_map (fun { Guard.RolePermission.model; permission; _ } ->
      if target == model then Some permission else None)
  in
  let role_permission permission =
    Guard.RolePermission.{ role; permission; model = target }
  in
  let events =
    let open Command in
    let open Data in
    [ read; delete ]
    |> to_urlencoded
    |> handle_booleans
    |> UpdateRolePermissions.decode
    >>= UpdateRolePermissions.handle role target current_permissions
  in
  let expected =
    Guard.
      [ RolePermissionSaved [ role_permission Permission.Delete ]
      ; RolePermissionDeleted (role_permission Permission.Update)
      ]
    |> Pool_event.(map guard)
    |> return
  in
  Test_utils.check_result expected events
;;
