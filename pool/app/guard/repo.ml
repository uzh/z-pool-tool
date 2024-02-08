open CCFun
module BaseRole = Role

module Backend =
  Guardian_backend.MariaDb.Make (Role.Actor) (Role.Role) (Role.Target)
    (Pool_database.GuardBackend)

include Backend

module RolePermission = struct
  include RolePermission

  let from_sql = {sql|
    guardian_role_permissions AS role_permissions
  |sql}

  let std_filter_sql = {sql| role_permissions.mark_as_deleted IS NULL |sql}

  let select_sql =
    {sql|
      role_permissions.role,
      role_permissions.permission,
      role_permissions.target_model
     |sql}
  ;;

  let select ?(count = false) fragment =
    if count
    then
      Format.sprintf
        {sql| SELECT COUNT(*) from guardian_role_permissions %s |sql}
        fragment
    else
      Format.sprintf
        "SELECT\n  %s\nFROM  %s\n  WHERE\n  %s\n %s"
        select_sql
        from_sql
        std_filter_sql
        fragment
  ;;

  let find_by query pool =
    Query.collect_and_count
      pool
      (Some query)
      ~select
      Backend.Entity.RolePermission.t
  ;;

  let insert pool = insert ~ctx:(Pool_database.to_ctx pool)
end

let src = Logs.Src.create "guard"

module Cache = struct
  (* TODO: Once the guardian package has a cached version, this implementation
     can be updated/removed (Issue:
     https://github.com/uzh/guardian/issues/11) *)
  open CCCache

  let equal_find_actor (l1, a1) (l2, a2) =
    Pool_database.Label.equal l1 l2 && Core.Uuid.Actor.equal a1 a2
  ;;

  let equal_validation (l1, s1, any1, a1) (l2, s2, any2, a2) =
    Pool_database.Label.equal l1 l2
    && Core.ValidationSet.equal s1 s2
    && CCBool.equal any1 any2
    && Core.Uuid.Actor.equal a1.Core.Actor.uuid a2.Core.Actor.uuid
  ;;

  let lru_find_by_actor = lru ~eq:equal_find_actor 2048
  let lru_find_actor = lru ~eq:equal_find_actor 2048
  let lru_validation = lru ~eq:equal_validation 2048

  let clear () =
    let () = clear lru_validation in
    let () = clear lru_find_by_actor in
    clear lru_find_actor
  ;;
end

module Actor = struct
  include Actor

  let find database_label id =
    let cb ~in_cache _ _ =
      if in_cache
      then (
        let tags = Pool_database.Logger.Tags.create database_label in
        Logs.debug ~src (fun m ->
          m ~tags "Found in cache: Actor %s" (id |> Core.Uuid.Actor.to_string)))
      else ()
    in
    let find' (label, id) = find ~ctx:(Pool_database.to_ctx label) id in
    (database_label, id) |> CCCache.(with_cache ~cb Cache.lru_find_actor find')
  ;;

  let can_assign_roles database_label actor =
    let open Utils.Lwt_result.Infix in
    ActorRole.permissions_of_actor
      ~ctx:(Pool_database.to_ctx database_label)
      actor.Core.Actor.uuid
    ||> CCList.filter_map
          (fun { Core.PermissionOnTarget.permission; model; target_uuid } ->
             let open Core in
             let has_permission =
               Permission.(equal Create permission || equal Manage permission)
             in
             let is_assign_role =
               CCList.exists
                 (Utils.find_assignable_target_role %> Role.Target.equal model)
                 Role.Role.all
             in
             if has_permission && is_assign_role
             then
               model
               |> Utils.find_assignable_role
               |> CCResult.map_or ~default:None (fun model ->
                 Some (model, target_uuid))
             else None)
  ;;

  let validate_assign_role database_label actor role =
    let%lwt possible_assigns = can_assign_roles database_label actor in
    let eq (r1, u1) (r2, u2) =
      Role.Role.equal r1 r2
      && (CCOption.(map2 Core.Uuid.Target.equal u1 u2 |> value ~default:false)
          || CCOption.is_none u2)
    in
    if CCList.mem ~eq role possible_assigns
    then Lwt.return_ok role
    else Lwt.return_error Pool_common.Message.PermissionDeniedGrantRole
  ;;
end

module ActorRole = struct
  include ActorRole
  open Caqti_request.Infix

  let find_by_actor_request =
    {sql|
      SELECT
        guardianDecodeUuid(role_targets.actor_uuid),
        role_targets.role,
        guardianDecodeUuid(role_targets.target_uuid),
        targets.model,
        COALESCE(
          exp.title,
          CONCAT(session_exp.title, ': Session at ', DATE_FORMAT(sessions.start, '%d.%m.%Y %H:%i')),
          locations.name
          )
      FROM guardian_actor_role_targets AS role_targets
      JOIN guardian_targets AS targets
        ON role_targets.target_uuid = targets.uuid
      LEFT JOIN pool_experiments AS exp
        ON role_targets.target_uuid = exp.uuid
      LEFT JOIN pool_sessions AS sessions
        ON role_targets.target_uuid = sessions.uuid
      LEFT JOIN pool_experiments AS session_exp
        ON sessions.experiment_uuid = session_exp.uuid
      LEFT JOIN pool_locations AS locations
        ON role_targets.target_uuid = locations.uuid
      WHERE role_targets.actor_uuid = guardianEncodeUuid($1)
        AND role_targets.mark_as_deleted IS NULL
      UNION
      SELECT guardianDecodeUuid(roles.actor_uuid), roles.role, NULL, NULL, NULL
      FROM guardian_actor_roles AS roles
      WHERE roles.actor_uuid = guardianEncodeUuid($1)
        AND roles.mark_as_deleted IS NULL
    |sql}
    |> Entity.(
         Uuid.Actor.t
         ->* Caqti_type.(t3 ActorRole.t (option TargetModel.t) (option string)))
  ;;

  let find_by_actor database_label actor =
    let cb ~in_cache _ _ =
      if in_cache
      then (
        let tags = Pool_database.Logger.Tags.create database_label in
        Logs.debug ~src (fun m ->
          m ~tags "Found in cache: Actor %s" (actor |> Core.Uuid.Actor.to_string)))
      else ()
    in
    let find_by_actor' (label, actor) =
      Utils.Database.collect
        (Pool_database.Label.value label)
        find_by_actor_request
        actor
    in
    (database_label, actor)
    |> CCCache.(with_cache ~cb Cache.lru_find_by_actor find_by_actor')
  ;;

  let permissions_of_actor database_label =
    permissions_of_actor ~ctx:(Pool_database.to_ctx database_label)
  ;;
end

let validate
  ?(any_id = false)
  database_label
  validation_set
  ({ Core.Actor.uuid; _ } as actor)
  =
  let cb ~in_cache _ _ =
    if in_cache
    then
      Logs.debug ~src (fun m ->
        m
          ~tags:(Pool_database.Logger.Tags.create database_label)
          "Found in cache: Actor %s\nValidation set %s"
          (uuid |> Core.Uuid.Actor.to_string)
          ([%show: Core.ValidationSet.t] validation_set))
    else ()
  in
  let validate' (label, set, any_id, actor) =
    validate
      ~ctx:(Pool_database.to_ctx label)
      ~any_id
      Pool_common.Message.authorization
      set
      actor
  in
  (database_label, validation_set, any_id, actor)
  |> CCCache.(with_cache ~cb Cache.lru_validation validate')
;;

open Pool_common.Message

let column_role = (Field.Role, "role_permissions.role") |> Query.Column.create

let column_model =
  (Field.Model, "role_permissions.target_model") |> Query.Column.create
;;

let column_action =
  (Field.Action, "role_permissions.permission") |> Query.Column.create
;;

let column_created_at =
  (Field.CreatedAt, "role_permissions.created_at") |> Query.Column.create
;;

let filterable_by = None
let searchable_by = [ column_role; column_model; column_action ]
let sortable_by = column_created_at :: searchable_by

let default_sort =
  Query.Sort.{ column = column_created_at; order = SortOrder.Descending }
;;

let default_query = Query.create ~sort:default_sort ()
