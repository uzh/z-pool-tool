module RepoEntity = Repo.RepoEntity
module Id = Pool_common.Repo.Id

type entity =
  | Experiment of Pool_common.Id.t
  | Session of Pool_common.Id.t
[@@deriving eq, show]

let get_id = function
  | Experiment id -> id
  | Session id -> id
;;

let insert_request =
  let open Caqti_request.Infix in
  {sql|
    INSERT INTO pool_participation_tags (
      entity_uuid,
      tag_uuid
    ) VALUES (
      UNHEX(REPLACE($1, '-', '')),
      UNHEX(REPLACE($2, '-', ''))
    )
    ON DUPLICATE KEY UPDATE
      updated_at = NOW()
  |sql}
  |> Caqti_type.(t2 Id.t Id.t ->. unit)
;;

let insert pool = Database.exec pool insert_request

let delete_request =
  let open Caqti_request.Infix in
  {sql|
    DELETE FROM pool_participation_tags
    WHERE entity_uuid = UNHEX(REPLACE($1, '-', ''))
    AND tag_uuid = UNHEX(REPLACE($2, '-', ''))
  |sql}
  |> Caqti_type.(t2 Id.t Id.t ->. unit)
;;

let delete pool = Database.exec pool delete_request

let find_all_request =
  let open Caqti_request.Infix in
  {sql|
    INNER JOIN pool_participation_tags
     	ON pool_tags.uuid = pool_participation_tags.tag_uuid
    WHERE pool_participation_tags.entity_uuid = UNHEX(REPLACE($1, '-', ''))
    |sql}
  |> Repo.Sql.find_request_sql
  |> Pool_common.Repo.Id.t ->* RepoEntity.t
;;

let find_all pool entity =
  Database.collect pool find_all_request (get_id entity)
;;

let find_available_for_experiment_request =
  let open Caqti_request.Infix in
  {sql|
    WHERE
    pool_tags.model = $1
    AND pool_tags.uuid NOT IN(
      SELECT
        tag_uuid FROM pool_participation_tags
      WHERE
        entity_uuid = UNHEX(REPLACE($2, '-', '')));
    |sql}
  |> Repo.Sql.find_request_sql
  |> Caqti_type.(t2 RepoEntity.Model.t Id.t) ->* RepoEntity.t
;;

let find_available_for_session_request =
  let open Caqti_request.Infix in
  {sql|
    WHERE
    pool_tags.model = $1
    AND pool_tags.uuid NOT IN(
      SELECT
        tag_uuid
      FROM
        pool_participation_tags
      WHERE
        entity_uuid = UNHEX(REPLACE($2, '-', ''))
        OR entity_uuid = (
          SELECT
            experiment_uuid
          FROM
            pool_sessions
          WHERE
            uuid = UNHEX(REPLACE($2, '-', ''))))
    |sql}
  |> Repo.Sql.find_request_sql
  |> Caqti_type.(t2 RepoEntity.Model.t Id.t) ->* RepoEntity.t
;;

let find_available pool entity =
  let model = Entity.Model.Contact in
  let collect fnc id = Database.collect pool fnc (model, id) in
  match entity with
  | Experiment id -> collect find_available_for_experiment_request id
  | Session id -> collect find_available_for_session_request id
;;
