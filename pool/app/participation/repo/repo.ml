module RepoEntity = Repo_entity

let of_entity = RepoEntity.of_entity
let to_entity = RepoEntity.to_entity

module Sql = struct
  let select_sql =
    {sql|
      SELECT
        LOWER(CONCAT(
          SUBSTR(HEX(pool_participations.uuid), 1, 8), '-',
          SUBSTR(HEX(pool_participations.uuid), 9, 4), '-',
          SUBSTR(HEX(pool_participations.uuid), 13, 4), '-',
          SUBSTR(HEX(pool_participations.uuid), 17, 4), '-',
          SUBSTR(HEX(pool_participations.uuid), 21)
        )),
        LOWER(CONCAT(
          SUBSTR(HEX(pool_sessions.uuid), 1, 8), '-',
          SUBSTR(HEX(pool_sessions.uuid), 9, 4), '-',
          SUBSTR(HEX(pool_sessions.uuid), 13, 4), '-',
          SUBSTR(HEX(pool_sessions.uuid), 17, 4), '-',
          SUBSTR(HEX(pool_sessions.uuid), 21)
        )),
        LOWER(CONCAT(
          SUBSTR(HEX(pool_subjects.uuid), 1, 8), '-',
          SUBSTR(HEX(pool_subjects.uuid), 9, 4), '-',
          SUBSTR(HEX(pool_subjects.uuid), 13, 4), '-',
          SUBSTR(HEX(pool_subjects.uuid), 17, 4), '-',
          SUBSTR(HEX(pool_subjects.uuid), 21)
        )),
        pool_participations.show_up,
        pool_participations.participated,
        pool_participations.matches_filter,
        pool_participations.chanceled_at,
        pool_participations.created_at,
        pool_participations.updated_at
      FROM
        pool_participations
      LEFT JOIN pool_sessions
        ON pool_participations.session_id = pool_sessions.id
      LEFT JOIN pool_subjects
        ON pool_participations.session_id = pool_subjects.id
    |sql}
  ;;

  let find_request =
    let open Caqti_request.Infix in
    {sql|
      WHERE
        uuid = UNHEX(REPLACE(?, '-', ''))
    |sql}
    |> Format.asprintf "%s\n%s" select_sql
    |> Caqti_type.string ->! RepoEntity.t
  ;;

  let find pool id =
    let open Lwt.Infix in
    Utils.Database.find_opt
      (Pool_database.Label.value pool)
      find_request
      (Pool_common.Id.value id)
    >|= CCOption.to_result Pool_common.Message.(NotFound Field.Tenant)
  ;;

  let find_by_session_request =
    let open Caqti_request.Infix in
    {sql|
      WHERE
        session_id = (SELECT id FROM pool_sessions WHERE uuid = UNHEX(REPLACE(?, '-', ''))),
    |sql}
    |> Format.asprintf "%s\n%s" select_sql
    |> Caqti_type.string ->* RepoEntity.t
  ;;

  let find_by_session pool id =
    Utils.Database.collect
      (Pool_database.Label.value pool)
      find_by_session_request
      (Pool_common.Id.value id)
  ;;

  let find_by_subject_request =
    let open Caqti_request.Infix in
    {sql|
      WHERE
        subject_id = (SELECT id FROM pool_subjects WHERE uuid = UNHEX(REPLACE(?, '-', ''))),
    |sql}
    |> Format.asprintf "%s\n%s" select_sql
    |> Caqti_type.string ->* RepoEntity.t
  ;;

  let find_by_subject pool id =
    Utils.Database.collect
      (Pool_database.Label.value pool)
      find_by_subject_request
      (Pool_common.Id.value id)
  ;;

  let insert_request =
    let open Caqti_request.Infix in
    {sql|
      INSERT INTO pool_participations (
        uuid,
        session_id,
        subject_id,
        show_up,
        participated,
        matches_filter,
        chanceled_at,
        created_at,
        updated_at
      ) VALUES (
        UNHEX(REPLACE($1, '-', '')),
        (SELECT id FROM pool_sessions WHERE pool_sessions.uuid = UNHEX(REPLACE($2, '-', ''))),
        (SELECT id FROM pool_subjects WHERE pool_subjects.uuid = UNHEX(REPLACE($3, '-', ''))),
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
          pool_participations
        SET
          show_up = $4,
          participated = $5,
          matches_filter = $6,
          canceled_at = $7
        WHERE
          uuid = UNHEX(REPLACE($1, '-', ''))
      |sql}
    |> Caqti_type.(
         tup2 string (tup2 bool (tup2 bool (tup2 bool (option ptime)))) ->. unit)
  ;;

  let format_update m =
    Entity.(
      ( m.id |> Pool_common.Id.value
      , ( m.show_up |> ShowUp.value
        , ( m.participated |> Participated.value
          , ( m.matches_filter |> MatchesFilter.value
            , m.canceled_at |> CanceledAt.value ) ) ) ))
  ;;

  let update pool m =
    Utils.Database.exec
      (Pool_database.Label.value pool)
      update_request
      (format_update m)
  ;;
end

let subject_to_participation pool participation =
  let open Utils.Lwt_result.Infix in
  Subject.find pool participation.RepoEntity.subject_id
  >|= to_entity participation
;;

let find pool id =
  let open Utils.Lwt_result.Infix in
  (* TODO Implement as transaction *)
  Sql.find pool id >>= subject_to_participation pool
;;

let find_by_session pool id =
  let open Lwt.Infix in
  (* TODO Implement as transaction *)
  Sql.find_by_session pool id
  >>= Lwt_list.map_s (subject_to_participation pool)
  |> Lwt.map CCList.all_ok
;;

let find_by_subject pool subject =
  let open Lwt.Infix in
  (* TODO Implement as transaction *)
  subject
  |> Subject.id
  |> Sql.find_by_subject pool
  (* Reload subject from DB, does not allow already made updates of the provided
     subject record *)
  >>= Lwt_list.map_s (subject_to_participation pool)
  |> Lwt.map CCList.all_ok
;;

let insert pool session_id model =
  model |> of_entity session_id |> Sql.insert pool
;;

let update = Sql.update
