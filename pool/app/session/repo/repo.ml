module Database = Pool_database
module RepoEntity = Repo_entity

let of_entity = RepoEntity.of_entity
let to_entity = RepoEntity.to_entity

module Sql = struct
  let contact_was_invited_join =
    {sql|
      INNER JOIN pool_experiments
        ON pool_experiments.uuid = pool_sessions.experiment_uuid
      INNER JOIN pool_invitations
        ON pool_invitations.contact_id = (SELECT id FROM pool_contacts WHERE user_uuid = UNHEX(REPLACE(?, '-', '')))
        AND pool_experiments.id = pool_invitations.experiment_id
    |sql}
  ;;

  let find_sql where =
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
          pool_sessions.reminder_subject,
          pool_sessions.reminder_text,
          pool_sessions.reminder_lead_time,
          pool_sessions.reminder_sent_at,
          (SELECT count(pool_assignments.id) FROM pool_assignments WHERE session_id=pool_sessions.id),
          pool_sessions.canceled_at,
          pool_sessions.created_at,
          pool_sessions.updated_at
        FROM pool_sessions
        INNER JOIN pool_locations
          ON pool_locations.id = pool_sessions.location_id
      |sql}
    in
    Format.asprintf "%s %s" select where
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
          (SELECT count(pool_assignments.id) FROM pool_assignments WHERE session_id=pool_sessions.id),
          pool_sessions.canceled_at
        FROM pool_sessions
        INNER JOIN pool_locations
          ON pool_locations.id = pool_sessions.location_id
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
    (* TODO [aerben] order by what here? *)
    {sql|
      WHERE pool_sessions.experiment_uuid = UNHEX(REPLACE(?, '-', ''))
      ORDER BY pool_sessions.start
    |sql}
    |> find_sql
    |> Caqti_type.string ->* RepoEntity.t
  ;;

  let find_all_for_experiment pool id =
    Utils.Database.collect
      (Database.Label.value pool)
      find_all_for_experiment_request
      (Pool_common.Id.value id)
  ;;

  let find_public_request =
    let open Caqti_request.Infix in
    {sql|
        WHERE pool_sessions.uuid = UNHEX(REPLACE(?, '-', ''))
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
        ON pool_assignments.session_id = pool_sessions.id
      WHERE pool_assignments.uuid = UNHEX(REPLACE(?, '-', ''))
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

  let find_all_public_for_experiment_request =
    let open Caqti_request.Infix in
    {sql|
        WHERE experiment_uuid = UNHEX(REPLACE(?, '-', ''))
        ORDER BY start
      |sql}
    |> find_public_sql
    |> Caqti_type.string ->* RepoEntity.Public.t
  ;;

  let find_all_public_for_experiment pool id =
    Utils.Database.collect
      (Database.Label.value pool)
      find_all_public_for_experiment_request
      (Pool_common.Id.value id)
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
    |> Caqti_type.(string ->! tup2 Pool_common.Repo.Id.t string)
  ;;

  let find_experiment_id_and_title pool id =
    let open Utils.Lwt_result.Infix in
    Utils.Database.find_opt
      (Database.Label.value pool)
      find_experiment_id_and_title_request
      (Pool_common.Id.value id)
    ||> CCOption.to_result Pool_common.Message.(NotFound Field.Session)
  ;;

  let find_sessions_to_remind_request =
    let open Caqti_request.Infix in
    {sql|
    INNER JOIN pool_experiments
      ON pool_experiments.uuid = pool_sessions.experiment_uuid
    WHERE
      pool_sessions.reminder_sent_at IS NULL
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
    (* TODO [aerben] order by what here? *)
    {sql|
        WHERE pool_sessions.follow_up_to = UNHEX(REPLACE(?, '-', ''))
        ORDER BY pool_sessions.start
      |sql}
    |> find_sql
    |> Caqti_type.string ->* RepoEntity.t
  ;;

  let find_follow_ups pool id =
    Utils.Database.collect
      (Database.Label.value pool)
      find_follow_ups_request
      (Pool_common.Id.value id)
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
        location_id,
        max_participants,
        min_participants,
        overbook,
        reminder_subject,
        reminder_text,
        reminder_lead_time,
        reminder_sent_at,
        canceled_at
      ) VALUES (
        UNHEX(REPLACE($2, '-', '')),
        UNHEX(REPLACE($3, '-', '')),
        UNHEX(REPLACE($1, '-', '')),
        $4,
        $5,
        $6,
        (SELECT id FROM pool_locations WHERE uuid = UNHEX(REPLACE($7, '-', ''))),
        $8,
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
        location_id = (SELECT pool_locations.id FROM pool_locations WHERE pool_locations.uuid = UNHEX(REPLACE($6, '-', ''))),
        max_participants = $7,
        min_participants = $8,
        overbook = $9,
        reminder_subject = $10,
        reminder_text = $11,
        reminder_lead_time = $12,
        reminder_sent_at = $13,
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
end

let location_to_repo_entity pool session =
  let open Utils.Lwt_result.Infix in
  Pool_location.find pool session.RepoEntity.location_id >|+ to_entity session
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
  >|> Lwt_list.map_s (location_to_repo_entity pool)
  ||> CCResult.flatten_l
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

let find_follow_ups pool parent_session_id =
  let open Utils.Lwt_result.Infix in
  Sql.find_follow_ups pool parent_session_id
  >|> Lwt_list.map_s (location_to_repo_entity pool)
  ||> CCResult.flatten_l
;;

let find_experiment_id_and_title = Sql.find_experiment_id_and_title

let find_sessions_to_remind pool =
  let open Utils.Lwt_result.Infix in
  Sql.find_sessions_to_remind pool
  >>= fun sessions ->
  Lwt_list.map_s (location_to_repo_entity pool) sessions ||> CCResult.flatten_l
;;

let insert = Sql.insert
let update = Sql.update
let delete = Sql.delete
