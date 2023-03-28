module RepoEntity = Repo_entity

let to_entity = RepoEntity.to_entity
let of_entity = RepoEntity.of_entity

module Sql = struct
  let select_sql =
    {sql|
      SELECT
        LOWER(CONCAT(
          SUBSTR(HEX(pool_mailing.uuid), 1, 8), '-',
          SUBSTR(HEX(pool_mailing.uuid), 9, 4), '-',
          SUBSTR(HEX(pool_mailing.uuid), 13, 4), '-',
          SUBSTR(HEX(pool_mailing.uuid), 17, 4), '-',
          SUBSTR(HEX(pool_mailing.uuid), 21)
        )),
        LOWER(CONCAT(
          SUBSTR(HEX(pool_experiments.uuid), 1, 8), '-',
          SUBSTR(HEX(pool_experiments.uuid), 9, 4), '-',
          SUBSTR(HEX(pool_experiments.uuid), 13, 4), '-',
          SUBSTR(HEX(pool_experiments.uuid), 17, 4), '-',
          SUBSTR(HEX(pool_experiments.uuid), 21)
        )),
        pool_mailing.start,
        pool_mailing.end,
        pool_mailing.rate,
        pool_mailing.distribution,
        pool_mailing.created_at,
        pool_mailing.updated_at
      FROM
        pool_mailing
      LEFT JOIN pool_experiments
        ON pool_mailing.experiment_uuid = pool_experiments.uuid
    |sql}
  ;;

  let find_request =
    let open Caqti_request.Infix in
    {sql|
      WHERE
        pool_mailing.uuid = UNHEX(REPLACE(?, '-', ''))
    |sql}
    |> Format.asprintf "%s\n%s" select_sql
    |> RepoEntity.Id.t ->! RepoEntity.t
  ;;

  let find pool id =
    let open Utils.Lwt_result.Infix in
    Utils.Database.find_opt (Pool_database.Label.value pool) find_request id
    ||> CCOption.to_result Pool_common.Message.(NotFound Field.Mailing)
  ;;

  let find_by_experiment_request =
    let open Caqti_request.Infix in
    {sql|
      WHERE
        experiment_uuid = UNHEX(REPLACE(?, '-', ''))
      ORDER BY pool_mailing.start
    |sql}
    |> Format.asprintf "%s\n%s" select_sql
    |> Experiment.Repo.Id.t ->* RepoEntity.t
  ;;

  let find_by_experiment pool =
    Utils.Database.collect
      (Pool_database.Label.value pool)
      find_by_experiment_request
  ;;

  let find_current_request =
    let open Caqti_request.Infix in
    {sql|
      WHERE
        NOW() < pool_mailing.end AND NOW() > pool_mailing.start
      ORDER BY pool_mailing.start
    |sql}
    |> Format.asprintf "%s\n%s" select_sql
    |> Caqti_type.unit ->* RepoEntity.t
  ;;

  let find_current pool =
    Utils.Database.collect
      (Pool_database.Label.value pool)
      find_current_request
      ()
  ;;

  let find_overlaps_request =
    let open Caqti_request.Infix in
    {sql|
      WHERE
        $1 < pool_mailing.end AND $2 > pool_mailing.start AND pool_mailing.uuid != UNHEX(REPLACE($3, '-', ''))
      ORDER BY pool_mailing.start
    |sql}
    |> Format.asprintf "%s\n%s" select_sql
    |> Caqti_type.tup3 RepoEntity.StartAt.t RepoEntity.EndAt.t RepoEntity.Id.t
       ->* RepoEntity.t
  ;;

  let find_overlaps pool start_at end_at ignore_id =
    Utils.Database.collect
      (Pool_database.Label.value pool)
      find_overlaps_request
      (start_at, end_at, ignore_id)
  ;;

  let find_experiment_id_request =
    let open Caqti_request.Infix in
    {sql|
      SELECT
        LOWER(CONCAT(
          SUBSTR(HEX(pool_experiments.uuid), 1, 8), '-',
          SUBSTR(HEX(pool_experiments.uuid), 9, 4), '-',
          SUBSTR(HEX(pool_experiments.uuid), 13, 4), '-',
          SUBSTR(HEX(pool_experiments.uuid), 17, 4), '-',
          SUBSTR(HEX(pool_experiments.uuid), 21)
        ))
      FROM pool_mailing
        LEFT JOIN pool_experiments
        ON pool_mailing.experiment_uuid = pool_experiments.uuid
      WHERE
        pool_mailing.uuid = UNHEX(REPLACE(?, '-', ''))
    |sql}
    |> Repo_entity.Id.t ->! Experiment.Repo.Id.t
  ;;

  let find_experiment_id pool id =
    let open Utils.Lwt_result.Infix in
    Utils.Database.find_opt
      (Pool_database.Label.value pool)
      find_experiment_id_request
      id
    ||> CCOption.to_result Pool_common.Message.(NotFound Field.Experiment)
  ;;

  let insert_request =
    let open Caqti_request.Infix in
    {sql|
      INSERT INTO pool_mailing (
        uuid,
        experiment_uuid,
        start,
        end,
        rate,
        distribution,
        created_at,
        updated_at
      ) VALUES (
        UNHEX(REPLACE($1, '-', '')),
        UNHEX(REPLACE($2, '-', '')),
        $3,
        $4,
        $5,
        $6,
        $7,
        $8
      )
    |sql}
    |> RepoEntity.t ->. Caqti_type.unit
  ;;

  let insert pool =
    Utils.Database.exec (Pool_database.Label.value pool) insert_request
  ;;

  let update_request =
    let open Caqti_request.Infix in
    {sql|
      UPDATE pool_mailing
      SET
        start = $2,
        end = $3,
        rate = $4,
        distribution = $5
      WHERE uuid = UNHEX(REPLACE($1, '-', ''))
    |sql}
    |> RepoEntity.Update.t ->. Caqti_type.unit
  ;;

  let update pool =
    Utils.Database.exec (Pool_database.Label.value pool) update_request
  ;;

  let delete_request =
    let open Caqti_request.Infix in
    {sql|
      DELETE FROM pool_mailing
      WHERE uuid = UNHEX(REPLACE($1, '-', ''))
    |sql}
    |> RepoEntity.Id.t ->. Caqti_type.unit
  ;;

  let delete pool =
    Utils.Database.exec (Pool_database.Label.value pool) delete_request
  ;;
end

let find pool id =
  let open Utils.Lwt_result.Infix in
  Sql.find pool id >|+ to_entity
;;

let find_by_experiment pool id =
  let open Utils.Lwt_result.Infix in
  Sql.find_by_experiment pool id ||> CCList.map to_entity
;;

let find_overlaps pool Entity.{ id; start_at; end_at; _ } =
  let open Utils.Lwt_result.Infix in
  Sql.find_overlaps pool start_at end_at id ||> CCList.map to_entity
;;

let find_current pool =
  let open Utils.Lwt_result.Infix in
  Sql.find_current pool ||> CCList.map to_entity
;;

let find_experiment_id = Sql.find_experiment_id

let insert pool experiment_id model =
  model |> of_entity experiment_id |> Sql.insert pool
;;

let update = Sql.update
let delete = Sql.delete
