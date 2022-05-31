module Database = Pool_database
module RepoEntity = Repo_entity

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
          pool_sessions.start,
          pool_sessions.duration,
          pool_sessions.description,
          pool_sessions.max_participants,
          pool_sessions.min_participants,
          pool_sessions.overbook,
          (SELECT count(pool_assignments.id) FROM pool_assignments WHERE session_id=pool_sessions.id),
          pool_sessions.canceled_at,
          pool_sessions.created_at,
          pool_sessions.updated_at
        FROM pool_sessions
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
          pool_sessions.start,
          pool_sessions.duration,
          pool_sessions.description,
          pool_sessions.max_participants,
          pool_sessions.min_participants,
          pool_sessions.overbook,
          (SELECT count(pool_assignments.id) FROM pool_assignments WHERE session_id=pool_sessions.id),
          pool_sessions.canceled_at
        FROM pool_sessions
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
    let open Lwt.Infix in
    Utils.Database.find_opt
      (Database.Label.value pool)
      find_request
      (Pool_common.Id.value id)
    >|= CCOption.to_result Pool_common.Message.(NotFound Field.Session)
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
    let where_fragment =
      {sql|
        WHERE pool_sessions.uuid = UNHEX(REPLACE(?, '-', ''))
        ORDER BY start
      |sql}
    in
    Format.asprintf "%s %s" contact_was_invited_join where_fragment
    |> find_public_sql
    |> Caqti_type.(tup2 string string) ->! RepoEntity.Public.t
  ;;

  let find_public pool id contact =
    let open Lwt.Infix in
    Utils.Database.find_opt
      (Database.Label.value pool)
      find_public_request
      (Contact.id contact |> Pool_common.Id.value, Pool_common.Id.value id)
    >|= CCOption.to_result Pool_common.Message.(NotFound Field.Session)
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
    let open Lwt.Infix in
    Utils.Database.find_opt
      (Database.Label.value pool)
      find_public_by_assignment_request
      (Pool_common.Id.value id)
    >|= CCOption.to_result Pool_common.Message.(NotFound Field.Session)
  ;;

  let find_all_public_for_experiment_request =
    let open Caqti_request.Infix in
    let where_fragment =
      {sql|
        WHERE experiment_uuid = UNHEX(REPLACE(?, '-', ''))
        ORDER BY start
      |sql}
    in
    Format.asprintf "%s %s" contact_was_invited_join where_fragment
    |> find_public_sql
    |> Caqti_type.(tup2 string string) ->* RepoEntity.Public.t
  ;;

  let find_all_public_for_experiment pool contact id =
    Utils.Database.collect
      (Database.Label.value pool)
      find_all_public_for_experiment_request
      (Contact.id contact |> Pool_common.Id.value, Pool_common.Id.value id)
  ;;

  let insert_request =
    let open Caqti_request.Infix in
    {sql|
      INSERT INTO pool_sessions (
        uuid,
        experiment_uuid,
        start,
        duration,
        description,
        max_participants,
        min_participants,
        overbook,
        canceled_at
      ) VALUES (
        UNHEX(REPLACE($2, '-', '')),
        UNHEX(REPLACE($1, '-', '')),
        $3,
        $4,
        $5,
        $6,
        $7,
        $8,
        $9
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
        start = $2,
        duration = $3,
        description = $4,
        max_participants = $5,
        min_participants = $6,
        overbook = $7,
        canceled_at = $8
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

let find = Sql.find
let find_public = Sql.find_public
let find_all_for_experiment = Sql.find_all_for_experiment
let find_all_public_for_experiment = Sql.find_all_public_for_experiment
let find_public_by_assignment = Sql.find_public_by_assignment
let insert = Sql.insert
let update = Sql.update
let delete = Sql.delete
