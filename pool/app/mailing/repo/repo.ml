module RepoEntity = Repo_entity
module Dynparam = Utils.Database.Dynparam

let to_entity = RepoEntity.to_entity
let of_entity = RepoEntity.of_entity

module Sql = struct
  let sql_select_columns =
    [ Entity.Id.sql_select_fragment ~field:"pool_mailing.uuid"
    ; Entity.Id.sql_select_fragment ~field:"pool_mailing.experiment_uuid"
    ]
    @ [ "pool_mailing.`start`"
      ; "pool_mailing.`end`"
      ; "pool_mailing.`limit`"
      ; "pool_mailing.distribution"
      ; "pool_mailing.created_at"
      ; "pool_mailing.updated_at"
      ]
  ;;

  let find_request_sql ?(count = false) ?(additional_cols = []) where_fragment =
    let columns =
      if count
      then "COUNT(*)"
      else CCString.concat ", " (sql_select_columns @ additional_cols)
    in
    Format.asprintf
      {sql|SELECT %s FROM pool_mailing %s|sql}
      columns
      where_fragment
  ;;

  let count_column =
    {sql|
      (SELECT COUNT(invitation_uuid)
      FROM pool_mailing_invitations
      WHERE mailing_uuid =  pool_mailing.uuid) as invitation_count
    |sql}
  ;;

  let find_request =
    let open Caqti_request.Infix in
    {sql|
      WHERE
        pool_mailing.uuid = UNHEX(REPLACE(?, '-', ''))
    |sql}
    |> find_request_sql
    |> RepoEntity.Id.t ->! RepoEntity.t
  ;;

  let find pool id =
    let open Utils.Lwt_result.Infix in
    Utils.Database.find_opt (Pool_database.Label.value pool) find_request id
    ||> CCOption.to_result Pool_message.(Error.NotFound Field.Mailing)
  ;;

  let select_with_count = find_request_sql ~additional_cols:[ count_column ]

  let find_with_detail_request =
    let open Caqti_request.Infix in
    Format.asprintf
      {sql|
        WHERE
          pool_mailing.uuid = UNHEX(REPLACE(?, '-', ''))
      |sql}
    |> select_with_count
    |> RepoEntity.(Id.t ->! Caqti_type.t2 t InvitationCount.t)
  ;;

  let find_with_detail pool id =
    let open Utils.Lwt_result.Infix in
    Utils.Database.find_opt
      (Pool_database.Label.value pool)
      find_with_detail_request
      id
    ||> CCOption.to_result Pool_message.(Error.NotFound Field.Mailing)
  ;;

  let find_by_experiment_request =
    let open Caqti_request.Infix in
    {sql|
      WHERE
        experiment_uuid = UNHEX(REPLACE(?, '-', ''))
      ORDER BY pool_mailing.start
    |sql}
    |> find_request_sql
    |> Experiment.Repo.Entity.Id.t ->* RepoEntity.t
  ;;

  let find_by_experiment pool =
    Utils.Database.collect
      (Pool_database.Label.value pool)
      find_by_experiment_request
  ;;

  let find_by_experiment_with_count pool query id =
    let select = select_with_count in
    let where =
      let sql =
        {sql| pool_mailing.experiment_uuid = UNHEX(REPLACE(?, '-', '')) |sql}
      in
      let dyn =
        Dynparam.(
          empty |> add Pool_common.Repo.Id.t (Experiment.Id.to_common id))
      in
      sql, dyn
    in
    Query.collect_and_count
      pool
      query
      ~select
      ~where
      RepoEntity.(Caqti_type.t2 t InvitationCount.t)
  ;;

  let find_current_request =
    let open Caqti_request.Infix in
    {sql|
      WHERE
        NOW() < pool_mailing.end AND NOW() >= pool_mailing.start
      ORDER BY pool_mailing.start
    |sql}
    |> find_request_sql
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
    |> find_request_sql
    |> Caqti_type.t3 RepoEntity.StartAt.t RepoEntity.EndAt.t RepoEntity.Id.t
       ->* RepoEntity.t
  ;;

  let find_overlaps pool start_at end_at ignore_id =
    Utils.Database.collect
      (Pool_database.Label.value pool)
      find_overlaps_request
      (start_at, end_at, ignore_id)
  ;;

  let find_binary_experiment_id_sql =
    {sql|
      SELECT pool_mailing.experiment_uuid
      FROM pool_mailing
      WHERE pool_mailing.uuid = ?
    |sql}
  ;;

  let find_experiment_id_request =
    let open Caqti_request.Infix in
    {sql|
      SELECT
        LOWER(CONCAT(
          SUBSTR(HEX(pool_mailing.experiment_uuid), 1, 8), '-',
          SUBSTR(HEX(pool_mailing.experiment_uuid), 9, 4), '-',
          SUBSTR(HEX(pool_mailing.experiment_uuid), 13, 4), '-',
          SUBSTR(HEX(pool_mailing.experiment_uuid), 17, 4), '-',
          SUBSTR(HEX(pool_mailing.experiment_uuid), 21)
        ))
      WHERE
        pool_mailing.uuid = UNHEX(REPLACE(?, '-', ''))
    |sql}
    |> Repo_entity.Id.t ->! Experiment.Repo.Entity.Id.t
  ;;

  let find_experiment_id pool id =
    let open Utils.Lwt_result.Infix in
    Utils.Database.find_opt
      (Pool_database.Label.value pool)
      find_experiment_id_request
      id
    ||> CCOption.to_result Pool_message.(Error.NotFound Field.Experiment)
  ;;

  let insert_request =
    let open Caqti_request.Infix in
    {sql|
      INSERT INTO pool_mailing (
        uuid,
        experiment_uuid,
        `start`,
        `end`,
        `limit`,
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
        `start` = $2,
        `end` = $3,
        `limit` = $4,
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

  module Status = struct
    let find_current_request =
      let open Caqti_request.Infix in
      let open RepoEntity in
      Format.asprintf
        {sql|
          SELECT
            %s,
            LEAST(CEIL(`limit` / pool_mailing.n_runs), pool_mailing.`open`) as to_send,
            last_run
          FROM (
            SELECT *,
              GREATEST(ROUND(TIMESTAMPDIFF(SECOND, `start`, `end`) / $1) - 1, 1) AS n_runs,
              GREATEST(`limit` -
                (SELECT COUNT(*) FROM pool_mailing_invitations AS mi WHERE mi.mailing_uuid = pool_mailing.uuid),
                0) AS `open`,
              `end` <= TIMESTAMPADD(SECOND, $1, `start`) AS last_run
            FROM pool_mailing
          ) AS pool_mailing
          WHERE NOW() <= pool_mailing.`end`
            AND NOW() >= pool_mailing.`start`
            AND pool_mailing.`open`
          ORDER BY pool_mailing.`start`
        |sql}
        (CCString.concat ", " sql_select_columns)
      |> Caqti_type.ptime_span ->* Status.t
    ;;

    let find_current pool interval =
      Utils.Database.collect
        (Pool_database.Label.value pool)
        find_current_request
        interval
    ;;
  end
end

let find pool id =
  let open Utils.Lwt_result.Infix in
  Sql.find pool id >|+ to_entity
;;

let find_with_detail pool id =
  let open Utils.Lwt_result.Infix in
  Sql.find_with_detail pool id
  >|+ fun (model, count) -> model |> to_entity, count
;;

let find_by_experiment pool id =
  let open Utils.Lwt_result.Infix in
  Sql.find_by_experiment pool id ||> CCList.map to_entity
;;

let find_by_experiment_with_count pool query id =
  let open Utils.Lwt_result.Infix in
  Sql.find_by_experiment_with_count pool query id
  ||> fun (mailings, query) ->
  let mailings =
    mailings |> CCList.map (fun (model, count) -> model |> to_entity, count)
  in
  mailings, query
;;

let find_overlaps pool Entity.{ id; start_at; end_at; _ } =
  let open Utils.Lwt_result.Infix in
  Sql.find_overlaps pool start_at end_at id ||> CCList.map to_entity
;;

let find_experiment_id = Sql.find_experiment_id

let insert pool experiment_id model =
  model |> of_entity experiment_id |> Sql.insert pool
;;

let update = Sql.update
let delete = Sql.delete

module Status = struct
  let find_current = Sql.Status.find_current
end
