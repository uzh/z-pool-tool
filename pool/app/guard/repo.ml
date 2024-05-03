open CCFun
module BaseRole = Role
module Dynparam = Database.Dynparam

let create_tag = Database.Logger.Tags.create

include
  Guardian_backend.MariaDb.Make (Role.Actor) (Role.Role) (Role.Target)
    (Database.Guard)

module ActorPermission = struct
  include ActorPermission

  let from_sql = {sql| guardian_actor_permissions AS actor_permissions |sql}
  let std_filter_sql = {sql| actor_permissions.mark_as_deleted IS NULL |sql}

  let select_sql =
    [%string
      {sql|
        %{Pool_common.Id.sql_select_fragment ~field:"actor_permissions.actor_uuid"},
        actor_permissions.permission,
        actor_permissions.target_model,
        %{Pool_common.Id.sql_select_fragment ~field:"actor_permissions.target_uuid"},
        COALESCE(
            CONCAT(user.given_name, ' ', user.name, ' (', user.email, ')'),
            %{Pool_common.Id.sql_select_fragment ~field:"actor_permissions.actor_uuid"}
            ),
        targets.model,
        COALESCE(
            exp.title,
            CONCAT(session_exp.title, ': Session at ', DATE_FORMAT(sessions.start, '%d.%m.%Y %H:%i')),
            locations.name
            )
      |sql}]
  ;;

  let joins =
    {sql|
      LEFT JOIN pool_experiments AS exp
        ON actor_permissions.target_uuid = exp.uuid
      LEFT JOIN pool_sessions AS sessions
        ON actor_permissions.target_uuid = sessions.uuid
      LEFT JOIN pool_experiments AS session_exp
        ON sessions.experiment_uuid = session_exp.uuid
      LEFT JOIN pool_locations AS locations
        ON actor_permissions.target_uuid = locations.uuid
      LEFT JOIN user_users AS user
        ON actor_permissions.actor_uuid = user.uuid
      LEFT JOIN guardian_targets AS targets
        ON target_uuid = targets.uuid
    |sql}
  ;;

  let select ?(count = false) fragment =
    let select_sql = if count then {sql| COUNT(*) |sql} else select_sql in
    Format.sprintf
      "SELECT\n  %s\nFROM  %s\n %s\n WHERE\n  %s\n %s"
      select_sql
      from_sql
      joins
      std_filter_sql
      fragment
  ;;

  let expanded =
    Caqti_type.(
      t4
        Entity.ActorPermission.t
        string
        (option Entity.TargetModel.t)
        (option string))
  ;;

  let find_by query pool =
    Query.collect_and_count pool (Some query) ~select expanded
  ;;

  let insert pool = insert ~ctx:(Database.to_ctx pool)
end

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
    let select_sql = if count then {sql| COUNT(*) |sql} else select_sql in
    Format.sprintf "SELECT %s FROM %s %s" select_sql from_sql fragment
  ;;

  let find_by query pool =
    let where = std_filter_sql, Dynparam.(empty) in
    Query.collect_and_count
      pool
      (Some query)
      ~where
      ~select
      Entity.RolePermission.t
  ;;

  let query_by_role ?query ?(include_static_models = false) pool role =
    let where =
      ( Format.asprintf "role_permissions.role = ? AND %s" std_filter_sql
      , Dynparam.(empty |> add Caqti_type.string (Role.Role.show role)) )
    in
    let where =
      match include_static_models with
      | false ->
        let open Role.Target in
        let sql, dyn = where in
        let sql =
          static
          |> CCList.map (CCFun.const "?")
          |> CCString.concat ","
          |> Format.asprintf
               "%s AND role_permissions.target_model NOT IN (%s)"
               sql
        in
        let dyn =
          CCList.fold_left
            (fun dyn target ->
              dyn |> Dynparam.add Caqti_type.string (show target))
            dyn
            Role.Target.static
        in
        sql, dyn
      | true -> where
    in
    Query.collect_and_count pool query ~where ~select Entity.RolePermission.t
  ;;

  let insert pool = insert ~ctx:(Database.to_ctx pool)

  let find_by_target_and_permissions_request permissions =
    let select_from_actor_roles =
      {sql|
        SELECT
          guardian_actors.uuid AS uuid,
          guardian_role_permissions.target_model AS target_model,
          guardian_role_permissions.permission AS permission
        FROM
          guardian_actors
          INNER JOIN guardian_actor_roles ON guardian_actor_roles.actor_uuid = guardian_actors.uuid
            AND guardian_actor_roles.mark_as_deleted IS NULL
          INNER JOIN guardian_role_permissions ON guardian_role_permissions.role = guardian_actor_roles.role
            AND guardian_role_permissions.mark_as_deleted IS NULL
        |sql}
    in
    let select_from_actor_role_targets =
      {sql|
        SELECT
          guardian_actors.uuid AS uuid,
          guardian_role_permissions.target_model AS target_model,
          guardian_role_permissions.permission AS permission
        FROM
          guardian_actors
          INNER JOIN guardian_actor_role_targets ON guardian_actor_role_targets.actor_uuid = guardian_actors.uuid
            AND guardian_actor_role_targets.target_uuid = UNHEX(REPLACE($1, '-', ''))
            AND guardian_actor_role_targets.mark_as_deleted IS NULL
          INNER JOIN guardian_role_permissions ON guardian_role_permissions.role = guardian_actor_role_targets.role
            AND guardian_role_permissions.mark_as_deleted IS NULL
        |sql}
    in
    let select_from_actor_permissions =
      {sql|
        SELECT
          guardian_actors.uuid AS uuid,
          guardian_actor_permissions.target_model AS target_model,
          guardian_actor_permissions.permission AS permission
        FROM
          guardian_actors
          INNER JOIN guardian_actor_permissions ON guardian_actor_permissions.actor_uuid = guardian_actors.uuid
            AND guardian_actor_permissions.mark_as_deleted IS NULL
            AND(guardian_actor_permissions.target_uuid IS NULL
              OR guardian_actor_permissions.target_uuid = UNHEX(REPLACE($1, '-', '')))
      |sql}
    in
    let permissions =
      CCList.mapi (fun i _ -> "$" ^ CCInt.to_string (i + 3)) permissions
      |> CCString.concat ", "
    in
    Format.asprintf
      {sql|
        SELECT
          %s
        FROM (%s UNION %s UNION %s) AS actor_rules
        WHERE
          actor_rules.target_model = $2
          AND actor_rules.permission IN(%s)
        GROUP BY
          actor_rules.uuid
      |sql}
      (Pool_common.Id.sql_select_fragment ~field:"actor_rules.uuid")
      select_from_actor_roles
      select_from_actor_role_targets
      select_from_actor_permissions
      permissions
  ;;

  let find_actors_by_target_and_permissions pool target entity_uuid permissions =
    let open Caqti_request.Infix in
    let permissions =
      Entity.Permission.(Manage :: permissions |> CCList.uniq ~eq:equal)
    in
    let open Dynparam in
    let (Pack (pt, pv)) =
      let add_string = add Caqti_type.string in
      empty
      |> add_string (Pool_common.Id.value entity_uuid)
      |> add_string (Role.Target.show target)
      |> flip
           (CCList.fold_left (fun dyn permission ->
              dyn |> add_string (permission |> Entity.Permission.show)))
           permissions
    in
    let request =
      find_by_target_and_permissions_request permissions
      |> pt ->* Pool_common.Repo.Id.t
    in
    Database.collect pool request pv
  ;;

  let permissions_by_role_and_target_request =
    let open Caqti_request.Infix in
    {|
      SELECT
        permission
      FROM
        guardian_role_permissions
      WHERE
        role = ?
        AND target_model = ?
        AND mark_as_deleted IS NULL
    |}
    |> Caqti_type.(t2 string string ->* Entity.Permission.t)
  ;;

  let permissions_by_role_and_target database_label role target =
    Database.collect
      database_label
      permissions_by_role_and_target_request
      (Role.Role.show role, Role.Target.show target)
  ;;
end

let src = Logs.Src.create "guard"

module Cache = struct
  (* TODO: Once the guardian package has a cached version, this implementation
     can be updated/removed (Issue:
     https://github.com/uzh/guardian/issues/11) *)
  open CCCache

  let equal_find_actor (l1, a1) (l2, a2) =
    Database.Label.equal l1 l2 && Core.Uuid.Actor.equal a1 a2
  ;;

  let equal_validation (l1, s1, any1, a1) (l2, s2, any2, a2) =
    Database.Label.equal l1 l2
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
    let () = clear lru_find_actor in
    clear_cache ()
  ;;

  let log_cache_size cache label =
    Logs.info ~src (fun m ->
      m "Updated size of guard cache %s: %i" label (size cache))
  ;;
end

module Actor = struct
  include Actor

  let find database_label id =
    let cb ~in_cache _ _ =
      if in_cache
      then (
        let tags = create_tag database_label in
        Logs.debug ~src (fun m ->
          m ~tags "Found in cache: Actor %s" (id |> Core.Uuid.Actor.to_string)))
      else Cache.log_cache_size Cache.lru_find_actor "lru_find_actor"
    in
    let find' (label, id) = find ~ctx:(Database.to_ctx label) id in
    (database_label, id) |> CCCache.(with_cache ~cb Cache.lru_find_actor find')
  ;;

  let can_assign_roles database_label actor =
    let open Utils.Lwt_result.Infix in
    ActorRole.permissions_of_actor
      ~ctx:(Database.to_ctx database_label)
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
    else Lwt.return_error Pool_message.Error.PermissionDeniedGrantRole
  ;;
end

module ActorRole = struct
  include ActorRole
  open Caqti_request.Infix

  let find_by_actor_request =
    [%string
      {sql|
        SELECT
          %{Pool_common.Id.sql_select_fragment ~field:"role_targets.actor_uuid"},
          role_targets.role,
          %{Pool_common.Id.sql_select_fragment ~field:"role_targets.target_uuid"},
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
        WHERE role_targets.actor_uuid = %{Pool_common.Id.sql_value_fragment "$1"}
          AND role_targets.mark_as_deleted IS NULL
        UNION
        SELECT %{Pool_common.Id.sql_select_fragment ~field:"roles.actor_uuid"}, roles.role, NULL, NULL, NULL
        FROM guardian_actor_roles AS roles
        WHERE roles.actor_uuid = %{Pool_common.Id.sql_value_fragment "$1"}
          AND roles.mark_as_deleted IS NULL
      |sql}]
    |> Entity.(
         Uuid.Actor.t
         ->* Caqti_type.(t3 ActorRole.t (option TargetModel.t) (option string)))
  ;;

  let find_by_actor database_label actor =
    let cb ~in_cache _ _ =
      if in_cache
      then (
        let tags = create_tag database_label in
        Logs.debug ~src (fun m ->
          m ~tags "Found in cache: Actor %s" (actor |> Core.Uuid.Actor.to_string)))
      else Cache.log_cache_size Cache.lru_find_by_actor "lru_find_by_actor"
    in
    let find_by_actor' (label, actor) =
      Database.collect label find_by_actor_request actor
    in
    (database_label, actor)
    |> CCCache.(with_cache ~cb Cache.lru_find_by_actor find_by_actor')
  ;;

  let permissions_of_actor database_label =
    permissions_of_actor ~ctx:(Database.to_ctx database_label)
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
          ~tags:(create_tag database_label)
          "Found in cache: Actor %s\nValidation set %s"
          (uuid |> Core.Uuid.Actor.to_string)
          ([%show: Core.ValidationSet.t] validation_set))
    else Cache.log_cache_size Cache.lru_validation "lru_validation"
  in
  let validate' (label, set, any_id, actor) =
    validate
      ~ctx:(Database.to_ctx label)
      ~any_id
      Pool_message.Error.authorization
      set
      actor
  in
  (database_label, validation_set, any_id, actor)
  |> CCCache.(with_cache ~cb Cache.lru_validation validate')
;;

module Role = struct
  let find_by_actor_and_permission_request permissions role_targets =
    let add_arguments list =
      CCList.map (fun _ -> "?") list |> CCString.concat ","
    in
    Format.asprintf
      {sql|
        SELECT
          target_model
        FROM
          guardian_role_permissions
          INNER JOIN guardian_actor_roles
            ON guardian_role_permissions.role = guardian_actor_roles.role
            AND guardian_actor_roles.mark_as_deleted IS NULL
        WHERE
          actor_uuid = UNHEX(REPLACE(?, '-', ''))
          AND guardian_role_permissions.mark_as_deleted IS NULL
          AND permission IN(%s)
          AND target_model IN(%s)
        |sql}
      (add_arguments permissions)
      (add_arguments role_targets)
  ;;

  let find_by_actor_and_permission pool actor_id permissions =
    let open Utils.Lwt_result.Infix in
    let open Caqti_request.Infix in
    let permissions =
      Entity.Permission.(Manage :: permissions |> CCList.uniq ~eq:equal)
    in
    let targets =
      Role.Role.(
        customizable |> CCList.map Core.Utils.find_assignable_target_role)
    in
    let open Dynparam in
    let (Pack (pt, pv)) =
      let add_string = add Caqti_type.string in
      empty
      |> add_string (Pool_common.Id.value actor_id)
      |> flip
           (CCList.fold_left (fun dyn permission ->
              dyn |> add_string (permission |> Entity.Permission.show)))
           permissions
      |> flip
           (CCList.fold_left (fun dyn target ->
              dyn |> add_string (target |> Role.Target.show)))
           targets
    in
    let request =
      find_by_actor_and_permission_request permissions targets
      |> pt ->* Entity.TargetModel.t
    in
    Database.collect pool request pv
    ||> CCList.map Core.Utils.find_assignable_role
    ||> CCList.all_ok
  ;;
end
