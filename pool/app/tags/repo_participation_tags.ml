module RepoEntity = Repo.RepoEntity

type entity =
  | Experiment of Experiment.Id.t
  | Session of Session.Id.t

let to_common_id = function
  | Experiment id -> id |> Experiment.Id.to_common
  | Session id -> id |> Session.Id.to_common
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
  |> Caqti_type.(tup2 Pool_common.Repo.Id.t RepoEntity.Id.t ->. unit)
;;

let insert pool =
  Utils.Database.exec (Pool_database.Label.value pool) insert_request
;;

let delete_request =
  let open Caqti_request.Infix in
  {sql|
    DELETE FROM pool_participation_tags
    WHERE entity_uuid = UNHEX(REPLACE($1, '-', ''))
    AND tag_uuid = UNHEX(REPLACE($2, '-', ''))
  |sql}
  |> Caqti_type.(tup2 Pool_common.Repo.Id.t RepoEntity.Id.t ->. unit)
;;

let delete pool =
  Utils.Database.exec (Pool_database.Label.value pool) delete_request
;;

let find_all_request =
  let open Caqti_request.Infix in
  Format.asprintf
    {sql|
    %s
    INNER JOIN pool_participation_tags
     	ON pool_tags.uuid = pool_participation_tags.tag_uuid
    WHERE pool_participation_tags.entity_uuid = UNHEX(REPLACE($1, '-', ''))
    |sql}
    Repo.Sql.select_tag_sql
  |> Pool_common.Repo.Id.t ->* RepoEntity.t
;;

let find_all pool entity =
  Utils.Database.collect
    (Pool_database.Label.value pool)
    find_all_request
    (to_common_id entity)
;;

let find_available_for_experiment_request =
  let open Caqti_request.Infix in
  Format.asprintf
    {sql|
    %s
    WHERE
    pool_tags.model = $1
    AND pool_tags.uuid NOT IN(
      SELECT
        tag_uuid FROM pool_participation_tags
      WHERE
        entity_uuid = UNHEX(REPLACE($2, '-', '')));
    |sql}
    Repo.Sql.select_tag_sql
  |> Caqti_type.(tup2 RepoEntity.Model.t Experiment.Repo.Entity.Id.t)
     ->* RepoEntity.t
;;

let find_available_for_session_request =
  let open Caqti_request.Infix in
  Format.asprintf
    {sql|
    %s
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
    Repo.Sql.select_tag_sql
  |> Caqti_type.(tup2 RepoEntity.Model.t Session.Repo.Id.t) ->* RepoEntity.t
;;

let find_available pool entity =
  let model = Entity.Model.Contact in
  let collect fnc id =
    Utils.Database.collect (Pool_database.Label.value pool) fnc (model, id)
  in
  match entity with
  | Experiment id -> collect find_available_for_experiment_request id
  | Session id -> collect find_available_for_session_request id
;;
