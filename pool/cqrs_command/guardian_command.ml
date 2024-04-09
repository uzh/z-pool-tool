let src = Logs.Src.create "guardian.cqrs"

module CreateRolePermission : sig
  include Common.CommandSig with type t = Guard.RolePermission.t

  val handle
    :  ?tags:Logs.Tag.set
    -> Guard.RolePermission.t
    -> (Pool_event.t list, Pool_common.Message.error) result
end = struct
  type t = Guard.RolePermission.t

  let handle ?(tags = Logs.Tag.empty) role_permission =
    Logs.info ~src (fun m -> m "Handle command CreateRolePermission" ~tags);
    Ok [ Guard.RolePermissionSaved [ role_permission ] |> Pool_event.guard ]
  ;;

  let effects = Guard.Access.Permission.manage
end

module DeleteRolePermission : sig
  include Common.CommandSig with type t = Guard.RolePermission.t

  val handle
    :  ?tags:Logs.Tag.set
    -> Guard.RolePermission.t
    -> (Pool_event.t list, Pool_common.Message.error) result
end = struct
  type t = Guard.RolePermission.t

  let handle ?(tags = Logs.Tag.empty) role_permission =
    Logs.info ~src (fun m -> m "Handle command DeleteRolePermission" ~tags);
    Ok [ Guard.RolePermissionDeleted role_permission |> Pool_event.guard ]
  ;;

  let effects = Guard.Access.Permission.manage
end

module UpdateRolePermissions : sig
  include Common.CommandSig with type t = (Guard.Permission.t * bool) list

  val handle
    :  ?tags:Logs.Tag.set
    -> Role.Role.t
    -> Role.Target.t
    -> Guard.Permission.t list
    -> t
    -> (Pool_event.t list, Pool_common.Message.error) result

  val decode
    :  (string * string list) list
    -> ((Guard.Permission.t * bool) list, Pool_common.Message.error) result
end = struct
  type t = (Guard.Permission.t * bool) list

  let decode data =
    let open CCList in
    let open Guard.Permission in
    all
    |> filter_map (fun permission ->
      assoc_opt ~eq:CCString.equal (show permission) data
      |> CCOption.map (fun value ->
        permission, value |> hd |> Utils.Bool.of_string))
    |> CCResult.return
  ;;

  let handle
    ?(tags = Logs.Tag.empty)
    role
    model
    current_permissions
    new_permissions
    =
    Logs.info ~src (fun m -> m "Handle command UpdateRolePermissions" ~tags);
    (* TODO: Currently I am assuming that the current user is allowed to update
       all permissions that were subitted *)
    let create, destroy =
      let open Guard in
      new_permissions
      |> CCList.partition_filter_map (fun (permission, value) ->
        let role_permission = RolePermission.{ role; permission; model } in
        CCList.mem ~eq:Permission.equal permission current_permissions
        |> function
        | false -> if value then `Left role_permission else `Drop
        | true -> if value then `Drop else `Right role_permission)
    in
    Guard.RolePermissionSaved create
    :: (destroy |> CCList.map Guard.rolepermissiondeleted)
    |> CCList.map Pool_event.guard
    |> CCResult.return
  ;;

  let effects = Guard.Access.Permission.manage
end

module CreateActorPermission : sig
  include Common.CommandSig with type t = Guard.ActorPermission.t

  val handle
    :  ?tags:Logs.Tag.set
    -> Guard.ActorPermission.t
    -> (Pool_event.t list, Pool_common.Message.error) result
end = struct
  type t = Guard.ActorPermission.t

  let handle ?(tags = Logs.Tag.empty) permission =
    Logs.info ~src (fun m -> m "Handle command CreateActorPermission" ~tags);
    Ok [ Guard.ActorPermissionSaved [ permission ] |> Pool_event.guard ]
  ;;

  let effects = Guard.Access.Permission.manage
end

module DeleteActorPermission : sig
  include Common.CommandSig with type t = Guard.ActorPermission.t

  val handle
    :  ?tags:Logs.Tag.set
    -> Guard.ActorPermission.t
    -> (Pool_event.t list, Pool_common.Message.error) result
end = struct
  type t = Guard.ActorPermission.t

  let handle ?(tags = Logs.Tag.empty) permission =
    Logs.info ~src (fun m -> m "Handle command DeleteActorPermission" ~tags);
    Ok [ Guard.ActorPermissionDeleted permission |> Pool_event.guard ]
  ;;

  let effects = Guard.Access.Permission.manage
end
