open Core

let src = Logs.Src.create "guard.events"

let log_role_permission ~decode ~tags =
  let open CCFun in
  Lwt_result.map (fun success ->
    Logs.debug ~src (fun m ->
      m ~tags "Save permission successful: %s" (decode success));
    success)
  %> Lwt_result.map_error (fun err ->
    Logs.err ~src (fun m -> m ~tags "Save permission failed: %s" (decode err));
    err)
;;

type event =
  | DefaultRestored of RolePermission.t list
  | RolesGranted of ActorRole.t list
  | RolesRevoked of ActorRole.t list
  | RolePermissionSaved of RolePermission.t list
  | RolePermissionDeleted of RolePermission.t
  | ActorPermissionSaved of ActorPermission.t list
  | ActorPermissionDeleted of ActorPermission.t
[@@deriving eq, show]

let handle_event database_label : event -> unit Lwt.t =
  let open Utils.Lwt_result.Infix in
  let tags = Database.Logger.Tags.create database_label in
  let ctx = [ "pool", Database.Label.value database_label ] in
  function
  | DefaultRestored permissions ->
    let%lwt (_ : (RolePermission.t list, RolePermission.t list) result) =
      Repo.RolePermission.insert_all ~ctx permissions
      |> log_role_permission ~decode:[%show: RolePermission.t list] ~tags
    in
    Lwt.return_unit
  | RolesGranted actor_roles ->
    Lwt_list.iter_s (Repo.ActorRole.upsert ~ctx) actor_roles
  | RolesRevoked actor_roles ->
    Lwt_list.iter_s (Repo.ActorRole.delete ~ctx) actor_roles
  | RolePermissionSaved permissions ->
    let%lwt (_ : (RolePermission.t list, RolePermission.t list) result) =
      Repo.RolePermission.insert_all ~ctx permissions
      |> log_role_permission ~decode:[%show: RolePermission.t list] ~tags
    in
    Lwt.return_unit
  | RolePermissionDeleted permission ->
    let%lwt (_ : (unit, string) result) =
      Repo.RolePermission.delete ~ctx permission
      ||> Pool_common.Utils.with_log_result_error
            ~src
            ~tags
            Pool_message.Error.authorization
    in
    Lwt.return_unit
  | ActorPermissionSaved permissions ->
    let%lwt (_ : (ActorPermission.t list, ActorPermission.t list) result) =
      Repo.ActorPermission.insert_all ~ctx permissions
      |> log_role_permission ~decode:[%show: ActorPermission.t list] ~tags
    in
    Lwt.return_unit
  | ActorPermissionDeleted permission ->
    let%lwt (_ : (unit, string) result) =
      Repo.ActorPermission.delete ~ctx permission
      ||> Pool_common.Utils.with_log_result_error
            ~src
            ~tags
            Pool_message.Error.authorization
    in
    Lwt.return_unit
;;
