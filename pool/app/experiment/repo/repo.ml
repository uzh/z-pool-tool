module Database = Pool_database

module Sql = struct
  let insert_sql =
    {sql|
      INSERT INTO pool_experiments (
        uuid,
        title,
        public_title,
        description,
        filter,
        direct_registration_disabled,
        registration_disabled
      ) VALUES (
        UNHEX(REPLACE(?, '-', '')),
        ?,
        ?,
        ?,
        ?,
        ?,
        ?
      )
    |sql}
  ;;

  let insert_request =
    let open Caqti_request.Infix in
    insert_sql |> Repo_entity.Write.t ->. Caqti_type.unit
  ;;

  let insert pool =
    Utils.Database.exec (Database.Label.value pool) insert_request
  ;;

  let select_from_experiments_sql where_fragment =
    let select_from =
      {sql|
        SELECT
          LOWER(CONCAT(
            SUBSTR(HEX(uuid), 1, 8), '-',
            SUBSTR(HEX(uuid), 9, 4), '-',
            SUBSTR(HEX(uuid), 13, 4), '-',
            SUBSTR(HEX(uuid), 17, 4), '-',
            SUBSTR(HEX(uuid), 21)
          )),
          title,
          public_title,
          description,
          filter,
          direct_registration_disabled,
          registration_disabled,
          created_at,
          updated_at
        FROM pool_experiments
      |sql}
    in
    Format.asprintf "%s %s" select_from where_fragment
  ;;

  let find_all_request =
    let open Caqti_request.Infix in
    "" |> select_from_experiments_sql |> Caqti_type.unit ->* Repo_entity.t
  ;;

  let find_all pool =
    Utils.Database.collect (Pool_database.Label.value pool) find_all_request
  ;;

  let find_request =
    let open Caqti_request.Infix in
    {sql|
      WHERE uuid = UNHEX(REPLACE(?, '-', ''))
    |sql}
    |> select_from_experiments_sql
    |> Caqti_type.string ->! Repo_entity.t
  ;;

  let find pool id =
    let open Lwt.Infix in
    Utils.Database.find_opt
      (Pool_database.Label.value pool)
      find_request
      (id |> Pool_common.Id.value)
    >|= CCOption.to_result Pool_common.Message.(NotFound Field.Experiment)
  ;;

  let update_request =
    let open Caqti_request.Infix in
    {sql|
      UPDATE pool_experiments
      SET
        title = $2,
        public_title = $3,
        description = $4,
        filter = $5,
        direct_registration_disabled = $6,
        registration_disabled = $7
      WHERE
        uuid = UNHEX(REPLACE($1, '-', ''))
    |sql}
    |> Repo_entity.Write.t ->. Caqti_type.unit
  ;;

  let format_update (t : Entity.t) =
    let open Entity in
    ( t.id |> Pool_common.Id.value
    , t.title |> Title.value
    , t.public_title |> PublicTitle.value
    , t.description |> Description.value
    , t.filter )
  ;;

  let update pool =
    Utils.Database.exec (Database.Label.value pool) update_request
  ;;

  let destroy_request =
    let open Caqti_request.Infix in
    {sql|
      DELETE FROM pool_experiments
      WHERE uuid = UNHEX(REPLACE($1, '-', ''))
    |sql}
    |> Caqti_type.(string ->. unit)
  ;;

  let destroy pool id =
    Utils.Database.exec
      (Pool_database.Label.value pool)
      destroy_request
      (id |> Pool_common.Id.value)
  ;;
end

let find = Sql.find
let find_all = Sql.find_all
let insert = Sql.insert
let update = Sql.update
let destroy = Sql.destroy
