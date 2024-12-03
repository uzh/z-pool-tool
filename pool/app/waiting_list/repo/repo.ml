module RepoEntity = Repo_entity
module Database = Database
module Dynparam = Database.Dynparam

module Sql = struct
  let sql_select_columns =
    (Entity.Id.sql_select_fragment ~field:"pool_waiting_list.uuid"
     :: Contact.Repo.sql_select_columns)
    @ Experiment.Repo.sql_select_columns
    @ [ "pool_waiting_list.comment"
      ; "pool_waiting_list.created_at"
      ; "pool_waiting_list.updated_at"
      ]
  ;;

  let joins =
    Format.asprintf
      {sql|
        LEFT JOIN pool_contacts
          ON pool_waiting_list.contact_uuid = pool_contacts.user_uuid
        %s
        LEFT JOIN pool_experiments
          ON pool_waiting_list.experiment_uuid = pool_experiments.uuid
        %s
      |sql}
      Contact.Repo.joins
      Experiment.Repo.joins
  ;;

  let find_request_sql ?(count = false) where_fragment =
    let columns =
      if count then "COUNT(*)" else CCString.concat ", " sql_select_columns
    in
    Format.asprintf
      {sql|SELECT %s FROM pool_waiting_list %s %s|sql}
      columns
      joins
      where_fragment
  ;;

  let find_request =
    let open Caqti_request.Infix in
    {sql|
      WHERE 
        pool_waiting_list.uuid = UNHEX(REPLACE(?, '-', ''))
      AND
        pool_waiting_list.marked_as_deleted = 0
    |sql}
    |> find_request_sql
    |> Caqti_type.string ->! RepoEntity.t
  ;;

  let find pool id =
    let open Utils.Lwt_result.Infix in
    Database.find_opt pool find_request (id |> Pool_common.Id.value)
    ||> CCOption.to_result Pool_message.(Error.NotFound Field.WaitingList)
  ;;

  let user_is_enlisted_request =
    let open Caqti_request.Infix in
    {sql|
      WHERE
        contact_uuid = UNHEX(REPLACE($1, '-', ''))
      AND
        experiment_uuid = UNHEX(REPLACE($2, '-', ''))
      AND 
        marked_as_deleted = 0
    |sql}
    |> find_request_sql
    |> Caqti_type.(t2 Contact.Repo.Id.t Experiment.Repo.Entity.Id.t)
       ->! RepoEntity.t
  ;;

  let find_by_contact_and_experiment pool contact experiment_id =
    Database.find_opt
      pool
      user_is_enlisted_request
      (contact |> Contact.id, experiment_id)
  ;;

  let find_by_contact_to_merge_request =
    let open Caqti_request.Infix in
    {sql| 
      WHERE contact_uuid = UNHEX(REPLACE($1, '-', ''))
      AND NOT EXISTS (
        SELECT 1
        FROM pool_waiting_list AS merge
        WHERE pool_waiting_list.experiment_uuid = merge.experiment_uuid
          AND merge.contact_uuid = UNHEX(REPLACE($2, '-', '')))|sql}
    |> find_request_sql
    |> Caqti_type.(t2 Contact.Repo.Id.t Contact.Repo.Id.t) ->* RepoEntity.t
  ;;

  let find_by_contact_to_merge pool ~contact ~merged_contact =
    let open Contact in
    Database.collect
      pool
      find_by_contact_to_merge_request
      (id contact, id merged_contact)
  ;;

  let find_by_experiment ?query pool id =
    let where =
      let sql =
        {sql|
          pool_waiting_list.experiment_uuid = UNHEX(REPLACE(?, '-', ''))
          AND pool_waiting_list.marked_as_deleted = 0
          AND NOT EXISTS (
            SELECT 1
            FROM pool_assignments
            INNER JOIN pool_sessions ON pool_assignments.session_uuid = pool_sessions.uuid
              AND pool_sessions.experiment_uuid = UNHEX(REPLACE(?, '-', ''))
            WHERE pool_assignments.contact_uuid = user_users.uuid
              AND pool_assignments.marked_as_deleted != 1)
        |sql}
      in
      let dyn =
        let open Dynparam in
        let add_id = add Pool_common.Repo.Id.t (Experiment.Id.to_common id) in
        empty |> add_id |> add_id
      in
      sql, dyn
    in
    Query.collect_and_count
      pool
      query
      ~select:find_request_sql
      ~where
      RepoEntity.t
  ;;

  let find_experiment_id_request =
    let open Caqti_request.Infix in
    {sql|
      SELECT
        LOWER(CONCAT(
          SUBSTR(HEX(pool_waiting_list.experiment_uuid), 1, 8), '-',
          SUBSTR(HEX(pool_waiting_list.experiment_uuid), 9, 4), '-',
          SUBSTR(HEX(pool_waiting_list.experiment_uuid), 13, 4), '-',
          SUBSTR(HEX(pool_waiting_list.experiment_uuid), 17, 4), '-',
          SUBSTR(HEX(pool_waiting_list.experiment_uuid), 21)
        ))
      FROM pool_waiting_list
      WHERE pool_waiting_list.uuid = UNHEX(REPLACE(?, '-', ''))
    |sql}
    |> Pool_common.Repo.Id.t ->! Experiment.Repo.Entity.Id.t
  ;;

  let find_experiment_id pool id =
    let open Utils.Lwt_result.Infix in
    Database.find_opt pool find_experiment_id_request id
    ||> CCOption.to_result Pool_message.(Error.NotFound Field.Experiment)
  ;;

  let insert_request =
    let open Caqti_request.Infix in
    {sql|
      INSERT INTO pool_waiting_list (
        uuid,
        contact_uuid,
        experiment_uuid,
        comment
      ) VALUES (
        UNHEX(REPLACE($1, '-', '')),
        UNHEX(REPLACE($2, '-', '')),
        UNHEX(REPLACE($3, '-', '')),
        $4
      ) ON DUPLICATE KEY UPDATE
        marked_as_deleted = 0
    |sql}
    |> Caqti_type.(RepoEntity.Write.t ->. unit)
  ;;

  let insert pool = Database.exec pool insert_request

  let update_request =
    let open Caqti_request.Infix in
    {sql|
      UPDATE pool_waiting_list
        SET
          comment = $1
        WHERE uuid = UNHEX(REPLACE($2, '-', ''))
    |sql}
    |> Caqti_type.(t2 (option string) string ->. unit)
  ;;

  let update pool (m : Entity.t) =
    let open Entity in
    let caqti =
      m.admin_comment |> AdminComment.value, m.id |> Pool_common.Id.value
    in
    Database.exec pool update_request caqti
  ;;

  let delete_request =
    let open Caqti_request.Infix in
    {sql|
      UPDATE pool_waiting_list
      SET marked_as_deleted = 1
      WHERE uuid = UNHEX(REPLACE($1, '-', ''))
    |sql}
    |> Caqti_type.(string ->. unit)
  ;;

  let delete pool m =
    Database.exec pool delete_request (m.Entity.id |> Pool_common.Id.value)
  ;;
end

let find = Sql.find
let find_by_contact_and_experiment = Sql.find_by_contact_and_experiment
let find_by_contact_to_merge = Sql.find_by_contact_to_merge

let user_is_enlisted pool contact experiment_id =
  let open Utils.Lwt_result.Infix in
  Sql.find_by_contact_and_experiment pool contact experiment_id
  ||> CCOption.is_some
;;

let find_by_experiment = Sql.find_by_experiment
let find_experiment_id = Sql.find_experiment_id
let insert = Sql.insert
let update = Sql.update
let delete = Sql.delete
