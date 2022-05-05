module RepoEntity = Repo_entity
module Database = Pool_database

module Sql = struct
  let select_sql where_fragment =
    let select_from =
      {sql|
          SELECT
            LOWER(CONCAT(
              SUBSTR(HEX(pool_waiting_list.uuid), 1, 8), '-',
              SUBSTR(HEX(pool_waiting_list.uuid), 9, 4), '-',
              SUBSTR(HEX(pool_waiting_list.uuid), 13, 4), '-',
              SUBSTR(HEX(pool_waiting_list.uuid), 17, 4), '-',
              SUBSTR(HEX(pool_waiting_list.uuid), 21)
            )),
            LOWER(CONCAT(
              SUBSTR(HEX(pool_subjects.user_uuid), 1, 8), '-',
              SUBSTR(HEX(pool_subjects.user_uuid), 9, 4), '-',
              SUBSTR(HEX(pool_subjects.user_uuid), 13, 4), '-',
              SUBSTR(HEX(pool_subjects.user_uuid), 17, 4), '-',
              SUBSTR(HEX(pool_subjects.user_uuid), 21)
            )),
            LOWER(CONCAT(
              SUBSTR(HEX(pool_experiments.uuid), 1, 8), '-',
              SUBSTR(HEX(pool_experiments.uuid), 9, 4), '-',
              SUBSTR(HEX(pool_experiments.uuid), 13, 4), '-',
              SUBSTR(HEX(pool_experiments.uuid), 17, 4), '-',
              SUBSTR(HEX(pool_experiments.uuid), 21)
            )),
            pool_waiting_list.created_at,
            pool_waiting_list.updated_at
          FROM pool_waiting_list
          LEFT JOIN pool_subjects
            ON pool_waiting_list.subject_id = pool_subjects.id
          LEFT JOIN pool_experiments
            ON pool_waiting_list.experiment_id = pool_experiments.id
        |sql}
    in
    Format.asprintf "%s %s" select_from where_fragment
  ;;

  let find_request =
    let open Caqti_request.Infix in
    {sql|
      WHERE uuid = UNHEX(REPLACE(?, '-', ''))
    |sql}
    |> select_sql
    |> Caqti_type.string ->! RepoEntity.t
  ;;

  let find pool id =
    let open Lwt.Infix in
    Utils.Database.find_opt
      (Pool_database.Label.value pool)
      find_request
      (id |> Pool_common.Id.value)
    >|= CCOption.to_result Pool_common.Message.(NotFound Field.WaitingList)
  ;;

  let user_is_enlisted_request =
    (* TODO[timhub]: How to deal with uuid vs Id *)
    let open Caqti_request.Infix in
    {sql|
      WHERE
        subject_id = (SELECT id FROM pool_subjects WHERE user_uuid = UNHEX(REPLACE($1, '-', '')))
      AND
        experiment_id = (SELECT id FROM pool_experiments WHERE uuid = UNHEX(REPLACE($2, '-', '')))
    |sql}
    |> select_sql
    |> Caqti_type.(tup2 string string) ->! RepoEntity.t
  ;;

  let user_is_enlisted pool subject experiment =
    let open Lwt.Infix in
    Utils.Database.find_opt
      (Pool_database.Label.value pool)
      user_is_enlisted_request
      ( subject |> Subject.id |> Pool_common.Id.value
      , experiment.Experiment_type.id |> Pool_common.Id.value )
    >|= function
    | None -> false
    | Some _ -> true
  ;;

  let find_multiple_sql where_fragment =
    Format.asprintf
      "SELECT %s %s"
      {sql|
      LOWER(CONCAT(
        SUBSTR(HEX(pool_waiting_list.uuid), 1, 8), '-',
        SUBSTR(HEX(pool_waiting_list.uuid), 9, 4), '-',
        SUBSTR(HEX(pool_waiting_list.uuid), 13, 4), '-',
        SUBSTR(HEX(pool_waiting_list.uuid), 17, 4), '-',
        SUBSTR(HEX(pool_waiting_list.uuid), 21)
      )),
      LOWER(CONCAT(
        SUBSTR(HEX(user_users.uuid), 1, 8), '-',
        SUBSTR(HEX(user_users.uuid), 9, 4), '-',
        SUBSTR(HEX(user_users.uuid), 13, 4), '-',
        SUBSTR(HEX(user_users.uuid), 17, 4), '-',
        SUBSTR(HEX(user_users.uuid), 21)
      )),
      user_users.email,
      user_users.username,
      user_users.name,
      user_users.given_name,
      user_users.password,
      user_users.status,
      user_users.admin,
      user_users.confirmed,
      user_users.created_at,
      user_users.updated_at,
      pool_subjects.language,
      pool_subjects.paused,
      pool_subjects.verified,
      pool_subjects.num_invitations,
      pool_subjects.num_assignments,
      pool_waiting_list.created_at,
      pool_waiting_list.updated_at
    FROM
      pool_waiting_list
    LEFT JOIN pool_subjects
      ON pool_waiting_list.subject_id = pool_subjects.id
    LEFT JOIN user_users
      ON pool_subjects.user_uuid = user_users.uuid
    |sql}
      where_fragment
  ;;

  let find_by_experiment_request =
    let open Caqti_request.Infix in
    {sql|
      WHERE
        pool_waiting_list.experiment_id = (SELECT id FROM pool_experiments WHERE uuid = UNHEX(REPLACE(?, '-', '')))
    |sql}
    |> find_multiple_sql
    |> Caqti_type.string ->* RepoEntity.Experiment.t
  ;;

  let find_by_experiment pool id =
    Utils.Database.collect
      (Pool_database.Label.value pool)
      find_by_experiment_request
      (Pool_common.Id.value id)
  ;;

  let insert_request =
    let open Caqti_request.Infix in
    {sql|
      INSERT INTO pool_waiting_list (
        uuid,
        subject_id,
        experiment_id
      ) VALUES (
        UNHEX(REPLACE($1, '-', '')),
        (SELECT id FROM pool_subjects WHERE pool_subjects.user_uuid = UNHEX(REPLACE($2, '-', ''))),
        (SELECT id FROM pool_experiments WHERE pool_experiments.uuid = UNHEX(REPLACE($3, '-', '')))
      )
    |sql}
    |> Caqti_type.(tup3 string string string ->. unit)
  ;;

  let insert pool m =
    let caqti =
      ( m.RepoEntity.id |> Pool_common.Id.value
      , m.RepoEntity.subject_id |> Pool_common.Id.value
      , m.RepoEntity.experiment_id |> Pool_common.Id.value )
    in
    Utils.Database.exec (Pool_database.Label.value pool) insert_request caqti
  ;;

  let delete_request =
    let open Caqti_request.Infix in
    {sql|
      DELETE FROM pool_waiting_list
      WHERE
        subject_id = (SELECT id FROM pool_subjects WHERE user_uuid = UNHEX(REPLACE($1, '-', '')))
      AND
        experiment_id = (SELECT id FROM pool_experiments WHERE uuid = UNHEX(REPLACE($2, '-', '')))
    |sql}
    |> Caqti_type.(tup2 string string ->. unit)
  ;;

  let delete pool subject experiment =
    Utils.Database.exec
      (Database.Label.value pool)
      delete_request
      ( Subject.id subject |> Pool_common.Id.value
      , experiment.Experiment_type.id |> Pool_common.Id.value )
  ;;
end

let find pool id =
  let open Lwt_result.Syntax in
  let* waiting_list = Sql.find pool id in
  let* experiment =
    Experiment.find pool waiting_list.RepoEntity.experiment_id
  in
  let* subject = Subject.find pool waiting_list.RepoEntity.subject_id in
  RepoEntity.to_entity waiting_list subject experiment |> Lwt.return_ok
;;

let user_is_enlisted = Sql.user_is_enlisted

let find_by_experiment pool id =
  let open Lwt_result.Syntax in
  let%lwt entries = Sql.find_by_experiment pool id in
  let* experiment = Experiment.find pool id in
  Entity.ListByExperiment.{ waiting_list_entries = entries; experiment }
  |> Lwt.return_ok
;;

let insert = Sql.insert
let delete = Sql.delete
