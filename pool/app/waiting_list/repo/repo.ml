module RepoEntity = Repo_entity
module Database = Pool_database
module Dynparam = Utils.Database.Dynparam

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
              SUBSTR(HEX(pool_contacts.user_uuid), 1, 8), '-',
              SUBSTR(HEX(pool_contacts.user_uuid), 9, 4), '-',
              SUBSTR(HEX(pool_contacts.user_uuid), 13, 4), '-',
              SUBSTR(HEX(pool_contacts.user_uuid), 17, 4), '-',
              SUBSTR(HEX(pool_contacts.user_uuid), 21)
            )),
            LOWER(CONCAT(
              SUBSTR(HEX(pool_experiments.uuid), 1, 8), '-',
              SUBSTR(HEX(pool_experiments.uuid), 9, 4), '-',
              SUBSTR(HEX(pool_experiments.uuid), 13, 4), '-',
              SUBSTR(HEX(pool_experiments.uuid), 17, 4), '-',
              SUBSTR(HEX(pool_experiments.uuid), 21)
            )),
            pool_waiting_list.comment,
            pool_waiting_list.created_at,
            pool_waiting_list.updated_at
          FROM pool_waiting_list
          LEFT JOIN pool_contacts
            ON pool_waiting_list.contact_id = pool_contacts.id
          LEFT JOIN pool_experiments
            ON pool_waiting_list.experiment_id = pool_experiments.id
        |sql}
    in
    Format.asprintf "%s %s" select_from where_fragment
  ;;

  let find_request =
    let open Caqti_request.Infix in
    {sql|
      WHERE pool_waiting_list.uuid = UNHEX(REPLACE(?, '-', ''))
    |sql}
    |> select_sql
    |> Caqti_type.string ->! RepoEntity.t
  ;;

  let find pool id =
    let open Utils.Lwt_result.Infix in
    Utils.Database.find_opt
      (Pool_database.Label.value pool)
      find_request
      (id |> Pool_common.Id.value)
    ||> CCOption.to_result Pool_common.Message.(NotFound Field.WaitingList)
  ;;

  let user_is_enlisted_request =
    let open Caqti_request.Infix in
    {sql|
      WHERE
        contact_id = (SELECT id FROM pool_contacts WHERE user_uuid = UNHEX(REPLACE($1, '-', '')))
      AND
        experiment_id = (SELECT id FROM pool_experiments WHERE uuid = UNHEX(REPLACE($2, '-', '')))
    |sql}
    |> select_sql
    |> Caqti_type.(tup2 string string) ->! RepoEntity.t
  ;;

  let find_by_contact_and_experiment pool contact experiment =
    Utils.Database.find_opt
      (Pool_database.Label.value pool)
      user_is_enlisted_request
      ( contact |> Contact.id |> Pool_common.Id.value
      , experiment.Experiment.Public.id |> Experiment.Id.value )
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
      pool_contacts.language,
      pool_contacts.paused,
      pool_contacts.verified,
      pool_contacts.num_invitations,
      pool_contacts.num_assignments,
      pool_waiting_list.comment,
      pool_waiting_list.created_at,
      pool_waiting_list.updated_at
    FROM
      pool_waiting_list
    LEFT JOIN pool_contacts
      ON pool_waiting_list.contact_id = pool_contacts.id
    LEFT JOIN user_users
      ON pool_contacts.user_uuid = user_users.uuid
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

  let select_count =
    let select_from =
      {sql|
      SELECT COUNT(*)
        FROM
        pool_waiting_list
      LEFT JOIN pool_contacts
        ON pool_waiting_list.contact_id = pool_contacts.id
      LEFT JOIN user_users
        ON pool_contacts.user_uuid = user_users.uuid
      |sql}
    in
    Format.asprintf "%s %s" select_from
  ;;

  let find_by_experiment ?query pool id =
    let where =
      let sql =
        {sql| pool_waiting_list.experiment_id = (SELECT id FROM pool_experiments WHERE uuid = UNHEX(REPLACE(?, '-', ''))) |sql}
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
      ~select:find_multiple_sql
      ~count:select_count
      ~where
      RepoEntity.Experiment.t
  ;;

  let insert_request =
    let open Caqti_request.Infix in
    {sql|
      INSERT INTO pool_waiting_list (
        uuid,
        contact_id,
        experiment_id
      ) VALUES (
        UNHEX(REPLACE($1, '-', '')),
        (SELECT id FROM pool_contacts WHERE pool_contacts.user_uuid = UNHEX(REPLACE($2, '-', ''))),
        (SELECT id FROM pool_experiments WHERE pool_experiments.uuid = UNHEX(REPLACE($3, '-', '')))
      )
    |sql}
    |> Caqti_type.(tup3 string string string ->. unit)
  ;;

  let insert pool m =
    let caqti =
      ( m.RepoEntity.id |> Pool_common.Id.value
      , m.RepoEntity.contact_id |> Pool_common.Id.value
      , m.RepoEntity.experiment_id |> Experiment.Id.value )
    in
    Utils.Database.exec (Pool_database.Label.value pool) insert_request caqti
  ;;

  let update_request =
    let open Caqti_request.Infix in
    {sql|
      UPDATE pool_waiting_list
        SET
          comment = $1
        WHERE uuid = UNHEX(REPLACE($2, '-', ''))
    |sql}
    |> Caqti_type.(tup2 (option string) string ->. unit)
  ;;

  let update pool (m : Entity.t) =
    let open Entity in
    let caqti = m.comment |> Comment.value, m.id |> Pool_common.Id.value in
    Utils.Database.exec (Pool_database.Label.value pool) update_request caqti
  ;;

  let delete_request =
    let open Caqti_request.Infix in
    {sql|
      DELETE FROM pool_waiting_list
      WHERE
        uuid = UNHEX(REPLACE($1, '-', ''))
    |sql}
    |> Caqti_type.(string ->. unit)
  ;;

  let delete pool m =
    Utils.Database.exec
      (Database.Label.value pool)
      delete_request
      (m.Entity.id |> Pool_common.Id.value)
  ;;
end

let find pool id =
  let open Utils.Lwt_result.Infix in
  let* waiting_list = Sql.find pool id in
  let experiment_id = waiting_list.RepoEntity.experiment_id in
  let* experiment = Experiment.find pool experiment_id in
  let* contact = Contact.find pool waiting_list.RepoEntity.contact_id in
  RepoEntity.to_entity waiting_list contact experiment |> Lwt.return_ok
;;

let find_by_contact_and_experiment pool contact experiment =
  let open Utils.Lwt_result.Infix in
  let%lwt waiting_list =
    Sql.find_by_contact_and_experiment pool contact experiment
  in
  let* experiment = Experiment.find pool experiment.Experiment.Public.id in
  CCOption.map
    (fun waiting_list -> RepoEntity.to_entity waiting_list contact experiment)
    waiting_list
  |> Lwt.return_ok
;;

let user_is_enlisted pool contact experiment =
  let open Utils.Lwt_result.Infix in
  Sql.find_by_contact_and_experiment pool contact experiment
  ||> function
  | None -> false
  | Some _ -> true
;;

let find_by_experiment ?query pool id =
  let open Utils.Lwt_result.Infix in
  let%lwt entries, query = Sql.find_by_experiment ?query pool id in
  let* experiment = Experiment.find pool id in
  (Entity.ExperimentList.{ waiting_list_entries = entries; experiment }, query)
  |> Lwt.return_ok
;;

let insert = Sql.insert
let update = Sql.update
let delete = Sql.delete
