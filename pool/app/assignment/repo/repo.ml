module RepoEntity = Repo_entity

let of_entity = RepoEntity.of_entity
let to_entity = RepoEntity.to_entity

module Sql = struct
  let select_sql ?(joins = "") where =
    Format.asprintf
      {sql|
        SELECT
          LOWER(CONCAT(
            SUBSTR(HEX(pool_assignments.uuid), 1, 8), '-',
            SUBSTR(HEX(pool_assignments.uuid), 9, 4), '-',
            SUBSTR(HEX(pool_assignments.uuid), 13, 4), '-',
            SUBSTR(HEX(pool_assignments.uuid), 17, 4), '-',
            SUBSTR(HEX(pool_assignments.uuid), 21)
          )),
          LOWER(CONCAT(
            SUBSTR(HEX(pool_assignments.session_uuid), 1, 8), '-',
            SUBSTR(HEX(pool_assignments.session_uuid), 9, 4), '-',
            SUBSTR(HEX(pool_assignments.session_uuid), 13, 4), '-',
            SUBSTR(HEX(pool_assignments.session_uuid), 17, 4), '-',
            SUBSTR(HEX(pool_assignments.session_uuid), 21)
          )),
          LOWER(CONCAT(
            SUBSTR(HEX(pool_assignments.contact_uuid), 1, 8), '-',
            SUBSTR(HEX(pool_assignments.contact_uuid), 9, 4), '-',
            SUBSTR(HEX(pool_assignments.contact_uuid), 13, 4), '-',
            SUBSTR(HEX(pool_assignments.contact_uuid), 17, 4), '-',
            SUBSTR(HEX(pool_assignments.contact_uuid), 21)
          )),
          pool_assignments.no_show,
          pool_assignments.participated,
          pool_assignments.matches_filter,
          pool_assignments.canceled_at,
          pool_assignments.marked_as_deleted,
          pool_assignments.created_at,
          pool_assignments.updated_at
        FROM
          pool_assignments
        %s
        WHERE
        %s
    |sql}
      joins
      where
  ;;

  let select_public_sql ?(joins = "") where =
    Format.asprintf
      {sql|
        SELECT
          LOWER(CONCAT(
            SUBSTR(HEX(pool_assignments.uuid), 1, 8), '-',
            SUBSTR(HEX(pool_assignments.uuid), 9, 4), '-',
            SUBSTR(HEX(pool_assignments.uuid), 13, 4), '-',
            SUBSTR(HEX(pool_assignments.uuid), 17, 4), '-',
            SUBSTR(HEX(pool_assignments.uuid), 21)
          )),
          pool_assignments.canceled_at
        FROM
          pool_assignments
        %s
        WHERE
        %s
      |sql}
      joins
      where
  ;;

  let find_request =
    let open Caqti_request.Infix in
    {sql|
        pool_assignments.uuid = UNHEX(REPLACE(?, '-', ''))
    |sql}
    |> select_sql
    |> Caqti_type.string ->! RepoEntity.t
  ;;

  let find pool id =
    let open Utils.Lwt_result.Infix in
    Utils.Database.find_opt
      (Pool_database.Label.value pool)
      find_request
      (Pool_common.Id.value id)
    ||> CCOption.to_result Pool_common.Message.(NotFound Field.Tenant)
  ;;

  let find_by_session_request ?where_condition () =
    let open Caqti_request.Infix in
    let id_fragment =
      {sql|
        pool_assignments.session_uuid = UNHEX(REPLACE(?, '-', ''))
        AND
        pool_assignments.marked_as_deleted = 0
      |sql}
    in
    where_condition
    |> CCOption.map_or
         ~default:id_fragment
         (Format.asprintf "%s AND %s" id_fragment)
    |> select_sql
    |> Caqti_type.string ->* RepoEntity.t
  ;;

  let find_by_session ?where_condition pool id =
    Utils.Database.collect
      (Pool_database.Label.value pool)
      (find_by_session_request ?where_condition ())
      (Session.Id.value id)
  ;;

  let find_deleted_by_session_request () =
    let open Caqti_request.Infix in
    {sql|
        pool_assignments.session_uuid = UNHEX(REPLACE(?, '-', ''))
        AND
        pool_assignments.marked_as_deleted = 1
      |sql}
    |> select_sql
    |> Caqti_type.string ->* RepoEntity.t
  ;;

  let find_deleted_by_session pool id =
    Utils.Database.collect
      (Pool_database.Label.value pool)
      (find_deleted_by_session_request ())
      (Session.Id.value id)
  ;;

  let find_by_contact_request =
    let open Caqti_request.Infix in
    {sql|
      pool_assignments.contact_uuid = UNHEX(REPLACE(?, '-', ''))
      AND
      pool_assignments.marked_as_deleted = 0
    |sql}
    |> select_sql
    |> Caqti_type.string ->* RepoEntity.t
  ;;

  let find_by_contact pool id =
    Utils.Database.collect
      (Pool_database.Label.value pool)
      find_by_contact_request
      (Pool_common.Id.value id)
  ;;

  let find_by_experiment_and_contact_opt_request =
    let open Caqti_request.Infix in
    let joins =
      {sql|
        LEFT JOIN pool_sessions
        ON
          pool_assignments.session_uuid = pool_sessions.uuid
        AND
          pool_sessions.canceled_at IS NULL
      |sql}
    in
    {sql|
        pool_sessions.experiment_uuid = UNHEX(REPLACE(?, '-', ''))
      AND
        pool_assignments.contact_uuid = UNHEX(REPLACE(?, '-', ''))
      AND
        pool_assignments.marked_as_deleted = 0
    |sql}
    |> select_public_sql ~joins
    |> Caqti_type.(tup2 string string) ->* RepoEntity.Public.t
  ;;

  let find_by_experiment_and_contact_opt pool experiment_id contact =
    Utils.Database.collect
      (Pool_database.Label.value pool)
      find_by_experiment_and_contact_opt_request
      ( Experiment.Id.value experiment_id
      , Pool_common.Id.value (Contact.id contact) )
  ;;

  let find_with_follow_ups_request =
    let open Caqti_request.Infix in
    let joins =
      {sql|
        INNER JOIN pool_sessions
        ON pool_assignments.session_uuid = pool_sessions.uuid
      |sql}
    in
    {sql|
      pool_assignments.marked_as_deleted = 0
      AND(
        pool_assignments.uuid = UNHEX(REPLACE($1, '-', ''))
        OR(
          pool_sessions.follow_up_to = (SELECT session_uuid FROM pool_assignments WHERE pool_assignments.uuid = UNHEX(REPLACE($1, '-', '')))
        AND
          pool_assignments.contact_uuid = (SELECT contact_uuid FROM pool_assignments WHERE pool_assignments.uuid = UNHEX(REPLACE($1, '-', '')))
        ))
    |sql}
    |> select_sql ~joins
    |> Caqti_type.string ->* RepoEntity.t
  ;;

  let find_with_follow_ups pool id =
    Utils.Database.collect
      (Pool_database.Label.value pool)
      find_with_follow_ups_request
      (Entity.Id.value id)
  ;;

  let find_session_id_request =
    let open Caqti_request.Infix in
    {sql|
      SELECT
        LOWER(CONCAT(
          SUBSTR(HEX(session_uuid), 1, 8), '-',
          SUBSTR(HEX(session_uuid), 9, 4), '-',
          SUBSTR(HEX(session_uuid), 13, 4), '-',
          SUBSTR(HEX(session_uuid), 17, 4), '-',
          SUBSTR(HEX(session_uuid), 21)
        ))
      FROM pool_assignments
      WHERE uuid = UNHEX(REPLACE(?, '-', ''))
    |sql}
    |> Pool_common.Repo.Id.(t ->! t)
  ;;

  let find_session_id pool id =
    let open Utils.Lwt_result.Infix in
    Utils.Database.find_opt
      (Pool_database.Label.value pool)
      find_session_id_request
      id
    ||> CCOption.to_result Pool_common.Message.(NotFound Field.Experiment)
  ;;

  let insert_request =
    let open Caqti_request.Infix in
    {sql|
      INSERT INTO pool_assignments (
        uuid,
        session_uuid,
        contact_uuid,
        no_show,
        participated,
        matches_filter,
        canceled_at,
        created_at,
        updated_at
      ) VALUES (
        UNHEX(REPLACE($1, '-', '')),
        UNHEX(REPLACE($2, '-', '')),
        UNHEX(REPLACE($3, '-', '')),
        $4,
        $5,
        $6,
        $7,
        $9,
        $10
      )
      ON DUPLICATE KEY UPDATE
        marked_as_deleted = 0
    |sql}
    |> RepoEntity.t ->. Caqti_type.unit
  ;;

  let insert pool =
    Utils.Database.exec (Pool_database.Label.value pool) insert_request
  ;;

  let update_request =
    let open Caqti_request.Infix in
    {sql|
        UPDATE
          pool_assignments
        SET
          no_show = $2,
          participated = $3,
          matches_filter = $4,
          canceled_at = $5
        WHERE
          uuid = UNHEX(REPLACE($1, '-', ''))
      |sql}
    |> Caqti_type.(
         tup2
           string
           (tup2 (option bool) (tup2 (option bool) (tup2 bool (option ptime))))
         ->. unit)
  ;;

  let format_update m =
    Entity.(
      ( m.id |> Pool_common.Id.value
      , ( m.no_show
        , ( m.participated
          , ( m.matches_filter |> MatchesFilter.value
            , CCOption.map CanceledAt.value m.canceled_at ) ) ) ))
  ;;

  let update pool m =
    Utils.Database.exec
      (Pool_database.Label.value pool)
      update_request
      (format_update m)
  ;;

  let marked_as_deleted_request =
    let open Caqti_request.Infix in
    {sql|
        UPDATE
          pool_assignments
        SET
          marked_as_deleted = 1
        WHERE
          uuid = UNHEX(REPLACE($1, '-', ''))
      |sql}
    |> Caqti_type.(string ->. unit)
  ;;

  let marked_as_deleted pool id =
    Utils.Database.exec
      (Pool_database.Label.value pool)
      marked_as_deleted_request
      (id |> Entity.Id.value)
  ;;
end

let contact_to_assignment pool assignment =
  let open Utils.Lwt_result.Infix in
  Contact.find pool assignment.RepoEntity.contact_id >|+ to_entity assignment
;;

let find pool id =
  let open Utils.Lwt_result.Infix in
  (* TODO Implement as transaction *)
  Sql.find pool id >>= contact_to_assignment pool
;;

let uncanceled_condition = "pool_assignments.canceled_at IS NULL"

let find_by_session filter pool id =
  let open Utils.Lwt_result.Infix in
  (* TODO Implement as transaction *)
  let sql =
    match filter with
    | `All -> Sql.find_by_session pool id
    | `Uncanceled ->
      Sql.find_by_session ~where_condition:uncanceled_condition pool id
    | `Deleted -> Sql.find_deleted_by_session pool id
  in
  sql >|> Lwt_list.map_s (contact_to_assignment pool) ||> CCList.all_ok
;;

let find_by_contact pool contact =
  let open Utils.Lwt_result.Infix in
  (* TODO Implement as transaction *)
  contact
  |> Contact.id
  |> Sql.find_by_contact pool
  (* Reload contact from DB, does not allow already made updates of the provided
     contact record *)
  >|> Lwt_list.map_s (contact_to_assignment pool)
  ||> CCList.all_ok
;;

let find_with_follow_ups pool id =
  let open Utils.Lwt_result.Infix in
  Sql.find_with_follow_ups pool id
  >|> Lwt_list.map_s (contact_to_assignment pool)
  ||> CCList.all_ok
;;

let find_session_id = Sql.find_session_id

let insert pool session_id model =
  model |> of_entity session_id |> Sql.insert pool
;;

let update = Sql.update
let find_by_experiment_and_contact_opt = Sql.find_by_experiment_and_contact_opt
let marked_as_deleted = Sql.marked_as_deleted
