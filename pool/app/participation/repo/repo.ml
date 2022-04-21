module RepoEntity = Repo_entity

let of_entity = RepoEntity.of_entity
let to_entity = RepoEntity.to_entity

module Sql = struct
  let select_sql =
    {sql|
      SELECT
        LOWER(CONCAT(
          SUBSTR(HEX(uuid), 1, 8), '-',
          SUBSTR(HEX(uuid), 9, 4), '-',
          SUBSTR(HEX(uuid), 13, 4), '-',
          SUBSTR(HEX(uuid), 17, 4), '-',
          SUBSTR(HEX(uuid), 21)
        )),
        session_id,
        participant_id,
        show_up,
        participated,
        matches_filter,
        chanceled_at,
        created_at,
        updated_at
      FROM
        pool_participations
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
        session_id = (SELECT id FROM pool_experiments WHERE uuid = UNHEX(REPLACE(?, '-', ''))),
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

  let find_by_participant_request =
    let open Caqti_request.Infix in
    {sql|
      WHERE
        participant_id = (SELECT id FROM pool_participants WHERE uuid = UNHEX(REPLACE(?, '-', ''))),
    |sql}
    |> Format.asprintf "%s\n%s" select_sql
    |> Caqti_type.string ->* RepoEntity.t
  ;;

  let find_by_participant pool id =
    Utils.Database.collect
      (Pool_database.Label.value pool)
      find_by_participant_request
      (Pool_common.Id.value id)
  ;;

  let insert_request =
    let open Caqti_request.Infix in
    {sql|
      INSERT INTO pool_participations (
        uuid,
        session_id,
        participant_id,
        show_up,
        participated,
        matches_filter,
        chanceled_at,
        created_at,
        updated_at
      ) VALUES (
        UNHEX(REPLACE($1, '-', '')),
        (SELECT id FROM pool_sessions WHERE pool_sessions.uuid = UNHEX(REPLACE($2, '-', ''))),
        (SELECT id FROM pool_participants WHERE pool_participants.uuid = UNHEX(REPLACE($3, '-', ''))),
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
    |> RepoEntity.t ->. Caqti_type.unit
  ;;

  let update pool t =
    Utils.Database.exec (Pool_database.Label.value pool) update_request t
  ;;
end

let find pool id =
  let open Utils.Lwt_result.Syntax in
  (* TODO Implement as transaction *)
  let* participation = Sql.find pool id in
  let* participant =
    Participant.find pool participation.RepoEntity.participant_id
  in
  to_entity participation participant |> Lwt.return_ok
;;

let find_by_session pool id =
  let open Lwt.Infix in
  (* TODO Implement as transaction *)
  Sql.find_by_session pool id
  >>= Lwt_list.map_s (fun participation ->
          let open Utils.Lwt_result.Infix in
          Participant.find pool participation.RepoEntity.participant_id
          >|= to_entity participation)
  |> Lwt.map CCList.all_ok
;;

let find_by_participant pool participant =
  let open Lwt.Infix in
  (* TODO Implement as transaction *)
  participant
  |> Participant.id
  |> Sql.find_by_participant pool
  (* TODO Load participand from db *)
  >|= CCList.map (CCFun.flip to_entity participant)
;;

let insert pool session_id model =
  model |> of_entity session_id |> Sql.insert pool
;;

let update pool model =
  let open Utils.Lwt_result.Syntax in
  let* participation = model.Entity.id |> Sql.find pool in
  model
  |> of_entity participation.RepoEntity.session_id
  |> Sql.update pool
  |> Lwt_result.ok
;;
