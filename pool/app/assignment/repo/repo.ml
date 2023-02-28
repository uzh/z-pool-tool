module RepoEntity = Repo_entity

let of_entity = RepoEntity.of_entity
let to_entity = RepoEntity.to_entity

module Sql = struct
  let select_sql =
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
          SUBSTR(HEX(pool_sessions.uuid), 1, 8), '-',
          SUBSTR(HEX(pool_sessions.uuid), 9, 4), '-',
          SUBSTR(HEX(pool_sessions.uuid), 13, 4), '-',
          SUBSTR(HEX(pool_sessions.uuid), 17, 4), '-',
          SUBSTR(HEX(pool_sessions.uuid), 21)
        )),
        LOWER(CONCAT(
          SUBSTR(HEX(pool_contacts.user_uuid), 1, 8), '-',
          SUBSTR(HEX(pool_contacts.user_uuid), 9, 4), '-',
          SUBSTR(HEX(pool_contacts.user_uuid), 13, 4), '-',
          SUBSTR(HEX(pool_contacts.user_uuid), 17, 4), '-',
          SUBSTR(HEX(pool_contacts.user_uuid), 21)
        )),
        pool_assignments.show_up,
        pool_assignments.participated,
        pool_assignments.matches_filter,
        pool_assignments.canceled_at,
        pool_assignments.created_at,
        pool_assignments.updated_at
      FROM
        pool_assignments
      LEFT JOIN pool_sessions
        ON pool_assignments.session_id = pool_sessions.id
      LEFT JOIN pool_contacts
        ON pool_assignments.contact_id = pool_contacts.id
    |sql}
  ;;

  let select_public_sql =
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
      LEFT JOIN pool_sessions
        ON pool_assignments.session_id = pool_sessions.id
      LEFT JOIN pool_contacts
        ON pool_assignments.session_id = pool_contacts.id
    |sql}
  ;;

  let find_request =
    let open Caqti_request.Infix in
    {sql|
      WHERE
        pool_assignments.uuid = UNHEX(REPLACE(?, '-', ''))
    |sql}
    |> Format.asprintf "%s\n%s" select_sql
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
        WHERE
          session_id = (SELECT id FROM pool_sessions WHERE uuid = UNHEX(REPLACE(?, '-', '')))
        AND
          marked_as_deleted = 0
      |sql}
    in
    where_condition
    |> CCOption.map_or
         ~default:id_fragment
         (Format.asprintf "%s AND %s" id_fragment)
    |> Format.asprintf "%s\n%s" select_sql
    |> Caqti_type.string ->* RepoEntity.t
  ;;

  let find_by_session ?where_condition pool id =
    Utils.Database.collect
      (Pool_database.Label.value pool)
      (find_by_session_request ?where_condition ())
      (Pool_common.Id.value id)
  ;;

  let find_by_contact_request =
    let open Caqti_request.Infix in
    {sql|
      WHERE
        contact_id = (SELECT id FROM pool_contacts WHERE uuid = UNHEX(REPLACE(?, '-', '')))
      AND
        marked_as_deleted = 0
    |sql}
    |> Format.asprintf "%s\n%s" select_sql
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
    {sql|
      WHERE
        pool_sessions.experiment_uuid = UNHEX(REPLACE(?, '-', ''))
      AND
        pool_assignments.contact_id = (SELECT id FROM pool_contacts WHERE pool_contacts.user_uuid = UNHEX(REPLACE(?, '-', '')))
      AND
        marked_as_deleted = 0
    |sql}
    |> Format.asprintf "%s\n%s" select_public_sql
    |> Caqti_type.(tup2 string string) ->* RepoEntity.Public.t
  ;;

  let find_by_experiment_and_contact_opt pool experiment_id contact =
    Utils.Database.collect
      (Pool_database.Label.value pool)
      find_by_experiment_and_contact_opt_request
      ( Experiment.Id.value experiment_id
      , Pool_common.Id.value (Contact.id contact) )
  ;;

  let insert_request =
    let open Caqti_request.Infix in
    {sql|
      INSERT INTO pool_assignments (
        uuid,
        session_id,
        contact_id,
        show_up,
        participated,
        matches_filter,
        canceled_at,
        created_at,
        updated_at
      ) VALUES (
        UNHEX(REPLACE($1, '-', '')),
        (SELECT id FROM pool_sessions WHERE pool_sessions.uuid = UNHEX(REPLACE($2, '-', ''))),
        (SELECT id FROM pool_contacts WHERE pool_contacts.user_uuid = UNHEX(REPLACE($3, '-', ''))),
        $4,
        $5,
        $6,
        $7,
        $8,
        $9
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
        UPDATE
          pool_assignments
        SET
          show_up = $2,
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
      , ( m.show_up
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

let insert pool session_id model =
  model |> of_entity session_id |> Sql.insert pool
;;

let update = Sql.update
let find_by_experiment_and_contact_opt = Sql.find_by_experiment_and_contact_opt
let marked_as_deleted = Sql.marked_as_deleted
