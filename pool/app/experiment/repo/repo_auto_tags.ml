let insert_request =
  let open Caqti_request.Infix in
  {sql|
    INSERT INTO pool_experiments_auto_tags (
      experiment_uuid,
      tag_uuid
    ) VALUES (
      UNHEX(REPLACE($1, '-', '')),
      UNHEX(REPLACE($2, '-', ''))
    )
    ON DUPLICATE KEY UPDATE
      updated_at = NOW()
  |sql}
  |> Caqti_type.(tup2 Repo_entity.Id.t Tags.RepoEntity.Id.t ->. unit)
;;

let insert pool =
  Utils.Database.exec (Pool_database.Label.value pool) insert_request
;;

let delete_request =
  let open Caqti_request.Infix in
  {sql|
    DELETE FROM pool_experiments_auto_tags
    WHERE experiment_uuid = UNHEX(REPLACE($1, '-', ''))
    AND tag_uuid = UNHEX(REPLACE($2, '-', ''))
  |sql}
  |> Caqti_type.(tup2 Repo_entity.Id.t Tags.RepoEntity.Id.t ->. unit)
;;

let delete pool =
  Utils.Database.exec (Pool_database.Label.value pool) delete_request
;;

let find_all_request =
  let open Caqti_request.Infix in
  Format.asprintf
    {sql|
    %s
    INNER JOIN pool_experiments_auto_tags
     	ON pool_tags.uuid = pool_experiments_auto_tags.tag_uuid
    WHERE pool_experiments_auto_tags.experiment_uuid = UNHEX(REPLACE($1, '-', ''))
    |sql}
    Tags.Sql.select_tag_sql
  |> Repo_entity.Id.t ->* Tags.RepoEntity.t
;;

let find_all pool =
  Utils.Database.collect (Pool_database.Label.value pool) find_all_request
;;

let find_available_request =
  let open Caqti_request.Infix in
  Format.asprintf
    {sql|
    %s
    WHERE
    pool_tags.model = $1
    AND pool_tags.uuid NOT IN(
      SELECT
        tag_uuid FROM pool_experiments_auto_tags
      WHERE
        experiment_uuid = UNHEX(REPLACE($2, '-', '')));
    |sql}
    Tags.Sql.select_tag_sql
  |> Caqti_type.(tup2 Tags.RepoEntity.Model.t Repo_entity.Id.t)
     ->* Tags.RepoEntity.t
;;

let find_available pool id =
  Utils.Database.collect
    (Pool_database.Label.value pool)
    find_available_request
    (Tags.Model.Experiment, id)
;;
