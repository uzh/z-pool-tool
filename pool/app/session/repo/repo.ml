module Database = Pool_database
module RepoEntity = Repo_entity
module Dynparam = Utils.Database.Dynparam

let of_entity = RepoEntity.of_entity
let to_entity = RepoEntity.to_entity

module Sql = struct
  let select_for_calendar ?order_by where =
    let order_by =
      order_by |> CCOption.map_or ~default:"" (Format.asprintf "ORDER BY %s")
    in
    Format.asprintf
      {sql|
      SELECT
        LOWER(CONCAT(
          SUBSTR(HEX(pool_sessions.uuid), 1, 8), '-',
          SUBSTR(HEX(pool_sessions.uuid), 9, 4), '-',
          SUBSTR(HEX(pool_sessions.uuid), 13, 4), '-',
          SUBSTR(HEX(pool_sessions.uuid), 17, 4), '-',
          SUBSTR(HEX(pool_sessions.uuid), 21)
        )),
        pool_experiments.title,
        LOWER(CONCAT(
          SUBSTR(HEX(pool_experiments.uuid), 1, 8), '-',
          SUBSTR(HEX(pool_experiments.uuid), 9, 4), '-',
          SUBSTR(HEX(pool_experiments.uuid), 13, 4), '-',
          SUBSTR(HEX(pool_experiments.uuid), 17, 4), '-',
          SUBSTR(HEX(pool_experiments.uuid), 21)
        )),
        pool_sessions.start,
        pool_sessions.duration,
        pool_sessions.description,
        pool_sessions.max_participants,
        pool_sessions.min_participants,
        pool_sessions.overbook,
        (SELECT
          COUNT(uuid)
        FROM
          pool_assignments
        WHERE
          pool_assignments.session_uuid = pool_sessions.uuid
          AND pool_assignments.canceled_at IS NULL
          AND pool_assignments.marked_as_deleted = 0),
        LOWER(CONCAT(
          SUBSTR(HEX(pool_locations.uuid), 1, 8), '-',
          SUBSTR(HEX(pool_locations.uuid), 9, 4), '-',
          SUBSTR(HEX(pool_locations.uuid), 13, 4), '-',
          SUBSTR(HEX(pool_locations.uuid), 17, 4), '-',
          SUBSTR(HEX(pool_locations.uuid), 21)
        )),
        pool_locations.name,
        user_users.given_name,
        user_users.name,
        user_users.email
      FROM pool_sessions
      INNER JOIN pool_experiments
        ON pool_sessions.experiment_uuid = pool_experiments.uuid
      INNER JOIN pool_locations
        ON pool_sessions.location_uuid = pool_locations.uuid
      LEFT JOIN user_users
        ON pool_experiments.contact_person_uuid = user_users.uuid
      WHERE
        %s
        %s
    |sql}
      where
      order_by
  ;;

  let find_sql ?order_by where =
    let order_by =
      order_by |> CCOption.map_or ~default:"" (Format.asprintf "ORDER BY %s")
    in
    let select =
      {sql|
        SELECT
          LOWER(CONCAT(
            SUBSTR(HEX(pool_sessions.uuid), 1, 8), '-',
            SUBSTR(HEX(pool_sessions.uuid), 9, 4), '-',
            SUBSTR(HEX(pool_sessions.uuid), 13, 4), '-',
            SUBSTR(HEX(pool_sessions.uuid), 17, 4), '-',
            SUBSTR(HEX(pool_sessions.uuid), 21)
          )),
          LOWER(CONCAT(
            SUBSTR(HEX(pool_sessions.follow_up_to), 1, 8), '-',
            SUBSTR(HEX(pool_sessions.follow_up_to), 9, 4), '-',
            SUBSTR(HEX(pool_sessions.follow_up_to), 13, 4), '-',
            SUBSTR(HEX(pool_sessions.follow_up_to), 17, 4), '-',
            SUBSTR(HEX(pool_sessions.follow_up_to), 21)
          )),
          (SELECT EXISTS (SELECT 1 FROM pool_sessions as s WHERE s.follow_up_to = pool_sessions.uuid LIMIT 1)),
          pool_sessions.start,
          pool_sessions.duration,
          pool_sessions.description,
          pool_sessions.limitations,
          LOWER(CONCAT(
            SUBSTR(HEX(pool_locations.uuid), 1, 8), '-',
            SUBSTR(HEX(pool_locations.uuid), 9, 4), '-',
            SUBSTR(HEX(pool_locations.uuid), 13, 4), '-',
            SUBSTR(HEX(pool_locations.uuid), 17, 4), '-',
            SUBSTR(HEX(pool_locations.uuid), 21)
          )),
          pool_sessions.max_participants,
          pool_sessions.min_participants,
          pool_sessions.overbook,
          pool_sessions.reminder_lead_time,
          pool_sessions.reminder_sent_at,
          COUNT(pool_assignments.id),
          COALESCE( SUM(pool_assignments.no_show), 0),
          COALESCE( SUM(pool_assignments.participated), 0),
          pool_sessions.closed_at,
          pool_sessions.canceled_at,
          pool_sessions.created_at,
          pool_sessions.updated_at
        FROM pool_sessions
        LEFT JOIN pool_assignments
          ON pool_assignments.session_uuid = pool_sessions.uuid
          AND pool_assignments.canceled_at IS NULL
          AND pool_assignments.marked_as_deleted = 0
        INNER JOIN pool_locations
          ON pool_locations.uuid = pool_sessions.location_uuid
      |sql}
    in
    Format.asprintf "%s %s GROUP BY pool_sessions.uuid %s" select where order_by
  ;;

  let find_public_sql where =
    let select =
      {sql|
        SELECT
          LOWER(CONCAT(
            SUBSTR(HEX(pool_sessions.uuid), 1, 8), '-',
            SUBSTR(HEX(pool_sessions.uuid), 9, 4), '-',
            SUBSTR(HEX(pool_sessions.uuid), 13, 4), '-',
            SUBSTR(HEX(pool_sessions.uuid), 17, 4), '-',
            SUBSTR(HEX(pool_sessions.uuid), 21)
          )),
          LOWER(CONCAT(
            SUBSTR(HEX(pool_sessions.follow_up_to), 1, 8), '-',
            SUBSTR(HEX(pool_sessions.follow_up_to), 9, 4), '-',
            SUBSTR(HEX(pool_sessions.follow_up_to), 13, 4), '-',
            SUBSTR(HEX(pool_sessions.follow_up_to), 17, 4), '-',
            SUBSTR(HEX(pool_sessions.follow_up_to), 21)
          )),
          pool_sessions.start,
          pool_sessions.duration,
          pool_sessions.description,
          LOWER(CONCAT(
            SUBSTR(HEX(pool_locations.uuid), 1, 8), '-',
            SUBSTR(HEX(pool_locations.uuid), 9, 4), '-',
            SUBSTR(HEX(pool_locations.uuid), 13, 4), '-',
            SUBSTR(HEX(pool_locations.uuid), 17, 4), '-',
            SUBSTR(HEX(pool_locations.uuid), 21)
          )),
          pool_sessions.max_participants,
          pool_sessions.min_participants,
          pool_sessions.overbook,
          (SELECT COUNT(pool_assignments.id) FROM pool_assignments WHERE session_uuid = pool_sessions.uuid AND marked_as_deleted = 0),
          pool_sessions.canceled_at
        FROM pool_sessions
        INNER JOIN pool_locations
          ON pool_locations.uuid = pool_sessions.location_uuid
      |sql}
    in
    Format.asprintf "%s %s" select where
  ;;

  let find_request =
    let open Caqti_request.Infix in
    {sql|
      WHERE pool_sessions.uuid = UNHEX(REPLACE(?, '-', ''))
    |sql}
    |> find_sql
    |> Caqti_type.string ->! RepoEntity.t
  ;;

  let find pool id =
    let open Utils.Lwt_result.Infix in
    Utils.Database.find_opt
      (Database.Label.value pool)
      find_request
      (Pool_common.Id.value id)
    ||> CCOption.to_result Pool_common.Message.(NotFound Field.Session)
  ;;

  let find_all_for_experiment_request =
    let open Caqti_request.Infix in
    {sql|
      WHERE pool_sessions.experiment_uuid = UNHEX(REPLACE(?, '-', ''))
    |sql}
    |> find_sql ~order_by:"pool_sessions.start"
    |> Caqti_type.string ->* RepoEntity.t
  ;;

  let find_all_for_experiment pool id =
    Utils.Database.collect
      (Database.Label.value pool)
      find_all_for_experiment_request
      (Experiment.Id.value id)
  ;;

  let find_all_to_assign_from_waitinglist_request =
    let open Caqti_request.Infix in
    {sql|
        WHERE pool_sessions.experiment_uuid = UNHEX(REPLACE(?, '-', ''))
        AND pool_sessions.start > NOW()
        AND pool_sessions.canceled_at IS NULL
      |sql}
    |> find_sql ~order_by:"pool_sessions.start"
    |> Caqti_type.string ->* RepoEntity.t
  ;;

  let find_all_to_assign_from_waitinglist pool id =
    Utils.Database.collect
      (Database.Label.value pool)
      find_all_to_assign_from_waitinglist_request
      (Experiment.Id.value id)
  ;;

  let find_public_request =
    let open Caqti_request.Infix in
    {sql|
        WHERE pool_sessions.uuid = UNHEX(REPLACE(?, '-', ''))
        AND start > NOW()
        AND canceled_at IS NULL
        ORDER BY start
      |sql}
    |> find_public_sql
    |> Caqti_type.string ->! RepoEntity.Public.t
  ;;

  let find_public pool id =
    let open Utils.Lwt_result.Infix in
    Utils.Database.find_opt
      (Database.Label.value pool)
      find_public_request
      (Pool_common.Id.value id)
    ||> CCOption.to_result Pool_common.Message.(NotFound Field.Session)
  ;;

  let find_public_by_assignment_request =
    let open Caqti_request.Infix in
    {sql|
      INNER JOIN pool_assignments
        ON pool_assignments.session_uuid = pool_sessions.uuid
      WHERE pool_assignments.uuid = UNHEX(REPLACE(?, '-', ''))
        AND pool_assignments.marked_as_deleted = 0
    |sql}
    |> find_public_sql
    |> Caqti_type.string ->! RepoEntity.Public.t
  ;;

  let find_public_by_assignment pool id =
    let open Utils.Lwt_result.Infix in
    Utils.Database.find_opt
      (Database.Label.value pool)
      find_public_by_assignment_request
      (Pool_common.Id.value id)
    ||> CCOption.to_result Pool_common.Message.(NotFound Field.Session)
  ;;

  let find_public_upcoming_by_contact_request =
    let open Caqti_request.Infix in
    {sql|
      INNER JOIN pool_assignments
        ON pool_assignments.session_uuid = pool_sessions.uuid
        AND pool_assignments.canceled_at IS NULL
        AND pool_assignments.marked_as_deleted = 0
      WHERE
        pool_sessions.closed_at IS NULL
      AND
        pool_sessions.start > NOW()
      AND
        pool_assignments.contact_uuid = UNHEX(REPLACE(?, '-', ''))
      ORDER BY
        pool_sessions.start ASC
    |sql}
    |> find_public_sql
    |> Caqti_type.string ->* RepoEntity.Public.t
  ;;

  let find_public_upcoming_by_contact pool contact_id =
    Utils.Database.collect
      (Database.Label.value pool)
      find_public_upcoming_by_contact_request
      (Pool_common.Id.value contact_id)
  ;;

  let find_by_assignment_request =
    let open Caqti_request.Infix in
    {sql|
      WHERE pool_assignments.uuid = UNHEX(REPLACE(?, '-', ''))
    |sql}
    |> find_sql
    |> Caqti_type.string ->! RepoEntity.t
  ;;

  let find_by_assignment pool id =
    let open Utils.Lwt_result.Infix in
    Utils.Database.find_opt
      (Database.Label.value pool)
      find_by_assignment_request
      (Pool_common.Id.value id)
    ||> CCOption.to_result Pool_common.Message.(NotFound Field.Session)
  ;;

  let find_all_public_for_experiment_request =
    let open Caqti_request.Infix in
    {sql|
      WHERE experiment_uuid = UNHEX(REPLACE(?, '-', ''))
      AND start > NOW()
      AND canceled_at IS NULL
      ORDER BY start
    |sql}
    |> find_public_sql
    |> Caqti_type.string ->* RepoEntity.Public.t
  ;;

  let find_all_public_for_experiment pool id =
    Utils.Database.collect
      (Database.Label.value pool)
      find_all_public_for_experiment_request
      (Experiment.Id.value id)
  ;;

  let find_all_public_by_location_request =
    let open Caqti_request.Infix in
    {sql|
      WHERE pool_locations.uuid = UNHEX(REPLACE(?, '-', ''))
      ORDER BY start
    |sql}
    |> find_public_sql
    |> Caqti_type.string ->* RepoEntity.Public.t
  ;;

  let find_all_public_by_location pool id =
    Utils.Database.collect
      (Database.Label.value pool)
      find_all_public_by_location_request
      (Pool_location.Id.value id)
  ;;

  let find_experiment_id_and_title_request =
    let open Caqti_request.Infix in
    {sql|
      SELECT
        LOWER(CONCAT(
          SUBSTR(HEX(pool_experiments.uuid), 1, 8), '-',
          SUBSTR(HEX(pool_experiments.uuid), 9, 4), '-',
          SUBSTR(HEX(pool_experiments.uuid), 13, 4), '-',
          SUBSTR(HEX(pool_experiments.uuid), 17, 4), '-',
          SUBSTR(HEX(pool_experiments.uuid), 21)
        )),
        pool_experiments.title
      FROM pool_sessions
      INNER JOIN pool_experiments
        ON pool_experiments.uuid = pool_sessions.experiment_uuid
      WHERE pool_sessions.uuid = UNHEX(REPLACE(?, '-', ''))
    |sql}
    |> Caqti_type.(string ->! tup2 Experiment.Repo.Entity.Id.t string)
  ;;

  let find_experiment_id_and_title pool id =
    let open Utils.Lwt_result.Infix in
    Utils.Database.find_opt
      (Database.Label.value pool)
      find_experiment_id_and_title_request
      (Pool_common.Id.value id)
    ||> CCOption.to_result Pool_common.Message.(NotFound Field.Session)
  ;;

  let find_public_experiment_request =
    let open Caqti_request.Infix in
    {sql|
      WHERE pool_experiments.uuid = (SELECT pool_sessions.experiment_uuid FROM pool_sessions WHERE pool_sessions.uuid = UNHEX(REPLACE(?, '-', '')))
    |sql}
    |> Experiment.Repo.Public.select_from_experiments_sql
    |> Caqti_type.(string ->! Experiment.Repo.Public.Entity.t)
  ;;

  let find_public_experiment pool id =
    let open Utils.Lwt_result.Infix in
    Utils.Database.find_opt
      (Database.Label.value pool)
      find_public_experiment_request
      (Pool_common.Id.value id)
    ||> CCOption.to_result Pool_common.Message.(NotFound Field.Experiment)
  ;;

  let find_sessions_to_remind_request =
    let open Caqti_request.Infix in
    {sql|
    INNER JOIN pool_experiments
      ON pool_experiments.uuid = pool_sessions.experiment_uuid
    WHERE
      pool_sessions.reminder_sent_at IS NULL
    AND
      pool_sessions.canceled_at IS NULL
    AND
      pool_sessions.closed_at IS NULL
    AND
      pool_sessions.start >= NOW()
    AND
      pool_sessions.start <= DATE_ADD(NOW(), INTERVAL
        COALESCE(
          pool_sessions.reminder_lead_time,
          pool_experiments.session_reminder_lead_time,
          (SELECT value FROM pool_system_settings WHERE settings_key = $1))
        SECOND)
    |sql}
    |> find_sql
    |> Caqti_type.(string) ->* RepoEntity.t
  ;;

  let find_sessions_to_remind pool =
    let settings_key = Settings.default_session_reminder_lead_time_key_yojson in
    Lwt_result.ok
    @@ Utils.Database.collect
         (Database.Label.value pool)
         find_sessions_to_remind_request
         (settings_key |> Yojson.Safe.to_string)
  ;;

  let find_follow_ups_request =
    let open Caqti_request.Infix in
    {sql|
        WHERE pool_sessions.follow_up_to = UNHEX(REPLACE(?, '-', ''))
        AND pool_sessions.canceled_at IS NULL
      |sql}
    |> find_sql ~order_by:"pool_sessions.start"
    |> Caqti_type.string ->* RepoEntity.t
  ;;

  let find_follow_ups pool id =
    Utils.Database.collect
      (Database.Label.value pool)
      find_follow_ups_request
      (Pool_common.Id.value id)
  ;;

  let find_open_request =
    let open Caqti_request.Infix in
    {sql|
        WHERE
          pool_sessions.closed_at IS NULL
        AND
          pool_sessions.canceled_at IS NULL
        AND
          pool_sessions.uuid = UNHEX(REPLACE($1, '-', ''))
      |sql}
    |> find_sql
    |> Caqti_type.string ->! RepoEntity.t
  ;;

  let find_open pool id =
    Utils.Database.find_opt
      (Database.Label.value pool)
      find_open_request
      (Pool_common.Id.value id)
  ;;

  let find_open_with_follow_ups_request =
    let open Caqti_request.Infix in
    {sql|
        WHERE
          pool_sessions.closed_at IS NULL
        AND
          pool_sessions.canceled_at IS NULL
        AND (
          pool_sessions.uuid = UNHEX(REPLACE($1, '-', ''))
          OR
          pool_sessions.follow_up_to = UNHEX(REPLACE($1, '-', ''))
        )
      |sql}
    |> find_sql ~order_by:"pool_sessions.start"
    |> Caqti_type.string ->* RepoEntity.t
  ;;

  let find_open_with_follow_ups pool id =
    Utils.Database.collect
      (Database.Label.value pool)
      find_open_with_follow_ups_request
      (Pool_common.Id.value id)
  ;;

  let find_binary_experiment_id_sql =
    {sql|
      SELECT sessions.experiment_uuid
      FROM pool_sessions AS sessions
      WHERE sessions.uuid = ?
    |sql}
  ;;

  let insert_request =
    let open Caqti_request.Infix in
    {sql|
      INSERT INTO pool_sessions (
        uuid,
        follow_up_to,
        experiment_uuid,
        start,
        duration,
        description,
        limitations,
        location_uuid,
        max_participants,
        min_participants,
        overbook,
        reminder_lead_time,
        reminder_sent_at,
        closed_at,
        canceled_at
      ) VALUES (
        UNHEX(REPLACE($2, '-', '')),
        UNHEX(REPLACE($3, '-', '')),
        UNHEX(REPLACE($1, '-', '')),
        $4,
        $5,
        $6,
        $7,
        UNHEX(REPLACE($8, '-', '')),
        $9,
        $10,
        $11,
        $12,
        $13,
        $14,
        $15
      )
    |sql}
    |> Caqti_type.(tup2 string RepoEntity.Write.t ->. unit)
  ;;

  let insert pool (experiment_id, session) =
    Utils.Database.exec
      (Database.Label.value pool)
      insert_request
      (experiment_id, session |> RepoEntity.Write.entity_to_write)
  ;;

  let update_request =
    let open Caqti_request.Infix in
    {sql|
      UPDATE pool_sessions
      SET
        follow_up_to = UNHEX(REPLACE($2, '-', '')),
        start = $3,
        duration = $4,
        description = $5,
        limitations = $6,
        location_uuid = UNHEX(REPLACE($7, '-', '')),
        max_participants = $8,
        min_participants = $9,
        overbook = $10,
        reminder_lead_time = $11,
        reminder_sent_at = $12,
        closed_at = $13,
        canceled_at = $14
      WHERE
        uuid = UNHEX(REPLACE($1, '-', ''))
    |sql}
    |> RepoEntity.Write.t ->. Caqti_type.unit
  ;;

  let update pool m =
    Utils.Database.exec
      (Database.Label.value pool)
      update_request
      (m |> RepoEntity.Write.entity_to_write)
  ;;

  let delete_request =
    let open Caqti_request.Infix in
    {sql|
      DELETE FROM pool_sessions
      WHERE uuid = UNHEX(REPLACE(?, '-', ''))
    |sql}
    |> Caqti_type.(string ->. unit)
  ;;

  let delete pool id =
    Utils.Database.exec
      (Pool_database.Label.value pool)
      delete_request
      (Pool_common.Id.value id)
  ;;

  let find_for_calendar_by_location_request =
    let open Caqti_request.Infix in
    {sql|
        pool_sessions.canceled_at IS NULL
        AND pool_sessions.start > $2
        AND pool_sessions.start < $3
        AND pool_sessions.location_uuid = UNHEX(REPLACE($1, '-', ''))
      |sql}
    |> select_for_calendar ~order_by:"pool_sessions.start"
    |> Caqti_type.(tup3 string ptime ptime ->* RepoEntity.Calendar.t)
  ;;

  let find_for_calendar_by_location location_id pool ~start_time ~end_time =
    Utils.Database.collect
      (Database.Label.value pool)
      find_for_calendar_by_location_request
      (Pool_location.Id.value location_id, start_time, end_time)
  ;;

  let find_for_calendar_by_user_request =
    select_for_calendar ~order_by:"pool_sessions.start"
  ;;

  let find_for_calendar_by_user actor pool ~start_time ~end_time =
    let open Caqti_request.Infix in
    let sql =
      [ "pool_sessions.start > ?"
      ; "pool_sessions.start < ?"
      ; "pool_sessions.canceled_at IS NULL"
      ; {sql|guardianValidateExperimentUuid(guardianEncodeUuid(?), ?, pool_experiments.uuid)|sql}
      ]
      |> CCString.concat " AND "
    in
    let dyn =
      let open Guard.Persistence in
      let open Dynparam in
      CCList.fold_left
        (fun dyn p -> dyn |> Dynparam.add Caqti_type.ptime p)
        empty
        [ start_time; end_time ]
      |> add Uuid.Actor.t (actor |> Guard.Actor.id)
      |> add Action.t Guard.Action.Read
    in
    let (Dynparam.Pack (pt, pv)) = dyn in
    let request =
      find_for_calendar_by_user_request sql
      |> (pt ->* RepoEntity.Calendar.t) ~oneshot:true
    in
    Utils.Database.collect (pool |> Pool_database.Label.value) request pv
  ;;
end

let location_to_repo_entity pool session =
  let open Utils.Lwt_result.Infix in
  Pool_location.find pool session.RepoEntity.location_id >|+ to_entity session
;;

let add_location_to_multiple pool sessions =
  let open Utils.Lwt_result.Infix in
  sessions
  |> Lwt_list.map_s (location_to_repo_entity pool)
  ||> CCResult.flatten_l
;;

let location_to_public_repo_entity pool session =
  let open Utils.Lwt_result.Infix in
  Pool_location.find pool session.RepoEntity.Public.location_id
  >|+ RepoEntity.Public.to_entity session
;;

let find pool id =
  let open Utils.Lwt_result.Infix in
  Sql.find pool id >>= location_to_repo_entity pool
;;

(* TODO [aerben] these queries are very inefficient, how to circumvent? *)
let find_all_public_by_location pool location_id =
  let open Utils.Lwt_result.Infix in
  Sql.find_all_public_by_location pool location_id
  >|> Lwt_list.map_s (location_to_public_repo_entity pool)
  ||> CCResult.flatten_l
;;

let find_public pool id =
  let open Utils.Lwt_result.Infix in
  id |> Sql.find_public pool >>= location_to_public_repo_entity pool
;;

let find_all_for_experiment pool experiment_id =
  let open Utils.Lwt_result.Infix in
  Sql.find_all_for_experiment pool experiment_id
  >|> add_location_to_multiple pool
;;

let find_all_to_assign_from_waitinglist pool experiment_id =
  let open Utils.Lwt_result.Infix in
  Sql.find_all_to_assign_from_waitinglist pool experiment_id
  >|> add_location_to_multiple pool
;;

let find_all_public_for_experiment pool contact experiment_id =
  let open Utils.Lwt_result.Infix in
  Experiment.find_public pool experiment_id contact
  >>= fun experiment ->
  experiment.Experiment.Public.id
  |> Sql.find_all_public_for_experiment pool
  >|> Lwt_list.map_s (location_to_public_repo_entity pool)
  ||> CCResult.flatten_l
;;

let find_public_by_assignment pool assignment_id =
  let open Utils.Lwt_result.Infix in
  Sql.find_public_by_assignment pool assignment_id
  >>= location_to_public_repo_entity pool
;;

let find_by_assignment pool assignment_id =
  let open Utils.Lwt_result.Infix in
  Sql.find_by_assignment pool assignment_id >>= location_to_repo_entity pool
;;

let find_follow_ups pool parent_session_id =
  let open Utils.Lwt_result.Infix in
  Sql.find_follow_ups pool parent_session_id >|> add_location_to_multiple pool
;;

let find_open_with_follow_ups pool session_id =
  let open Utils.Lwt_result.Infix in
  Sql.find_open_with_follow_ups pool session_id
  ||> (function
        | [] -> Error Pool_common.Message.(NotFound Field.Session)
        | sessions -> Ok sessions)
  >>= add_location_to_multiple pool
;;

let find_open pool session_id =
  let open Utils.Lwt_result.Infix in
  Sql.find_open pool session_id
  ||> CCOption.to_result Pool_common.Message.(NotFound Field.Session)
  >>= location_to_repo_entity pool
;;

let find_experiment_id_and_title = Sql.find_experiment_id_and_title

let find_upcoming_public_by_contact pool contact_id =
  let open Utils.Lwt_result.Infix in
  let open Entity.Public in
  Sql.find_public_upcoming_by_contact pool contact_id
  >|> Lwt_list.map_s (location_to_public_repo_entity pool)
  ||> CCResult.flatten_l
  >|+ group_and_sort_keep_followups
  >>= fun lst ->
  lst
  |> Lwt_list.map_s (fun (parent, follow_ups) ->
    Sql.find_public_experiment pool parent.id
    >|+ fun exp -> exp, parent, follow_ups)
  ||> CCResult.flatten_l
;;

let find_sessions_to_remind pool =
  let open Utils.Lwt_result.Infix in
  Sql.find_sessions_to_remind pool >>= add_location_to_multiple pool
;;

let insert = Sql.insert
let update = Sql.update
let delete = Sql.delete
let find_for_calendar_by_location = Sql.find_for_calendar_by_location
let find_for_calendar_by_user = Sql.find_for_calendar_by_user
