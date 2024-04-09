open CCFun

let src = Logs.Src.create "guardian.cqrs"

type grant_role =
  { target : Admin.t
  ; roles : (Role.Role.t * Guard.Uuid.Target.t option) list
  }

type revoke_role =
  { target : Admin.t
  ; role : Role.Role.t * Guard.Uuid.Target.t option
  }

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

(* TODO: Create and Delete required? *)
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

(* TODO: Create and Delete required? *)

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

(* TODO: Move this commands to admin_command.ml *)
module GrantRoles : sig
  include Common.CommandSig with type t = grant_role
end = struct
  open Guard

  type t = grant_role

  let handle ?(tags = Logs.Tag.empty) { target; roles } =
    Logs.info ~src (fun m -> m "Handle command GrantRoles" ~tags);
    let actor_roles =
      let to_id = Admin.id %> Guard.Uuid.actor_of Admin.Id.value in
      CCList.map
        (fun (role, target_uuid) ->
          Guard.ActorRole.create ?target_uuid (target |> to_id) role)
        roles
    in
    Ok
      [ Guard.RolesGranted actor_roles |> Pool_event.guard
      ; Common.guardian_cache_cleared_event ()
      ]
  ;;

  let effects = Guard.Access.Role.create
end

module RevokeRole : sig
  include Common.CommandSig with type t = revoke_role
end = struct
  open Guard

  type t = revoke_role

  let handle ?(tags = Logs.Tag.empty) { target; role } =
    Logs.info ~src (fun m -> m ~tags "Handle command RevokeRole");
    let actor_roles =
      let role, target_uuid = role in
      let to_id = Admin.id %> Guard.Uuid.actor_of Admin.Id.value in
      Guard.ActorRole.create ?target_uuid (target |> to_id) role
    in
    Ok
      [ Guard.RolesRevoked [ actor_roles ] |> Pool_event.guard
      ; Common.guardian_cache_cleared_event ()
      ]
  ;;

  let effects = Guard.Access.Role.delete
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
