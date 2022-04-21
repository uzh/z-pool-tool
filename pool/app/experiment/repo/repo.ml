module Database = Pool_database

module Sql = struct
  let insert_sql =
    {sql|
      INSERT INTO pool_experiments (
        uuid,
        title,
        description,
        filter,
        created_at,
        updated_at
      ) VALUES (
        UNHEX(REPLACE(?, '-', '')),
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
    insert_sql |> Repo_entity.t ->. Caqti_type.unit
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
          description,
          filter,
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
end

let find _ = Utils.todo
let find_all = Sql.find_all
let insert = Sql.insert
let update _ = Utils.todo
let destroy _ = Utils.todo
