module Database = Pool_database

module Sql = struct
  let select_filter_sql where_fragment =
    let select_from =
      {sql|
        SELECT
          LOWER(CONCAT(
            SUBSTR(HEX(pool_filter.uuid), 1, 8), '-',
            SUBSTR(HEX(pool_filter.uuid), 9, 4), '-',
            SUBSTR(HEX(pool_filter.uuid), 13, 4), '-',
            SUBSTR(HEX(pool_filter.uuid), 17, 4), '-',
            SUBSTR(HEX(pool_filter.uuid), 21)
          )),
          pool_filter.filter,
          pool_filter.created_at,
          pool_filter.updated_at
        FROM pool_filter
      |sql}
    in
    Format.asprintf "%s %s" select_from where_fragment
  ;;

  let find_request =
    let open Caqti_request.Infix in
    {sql|
      WHERE pool_filter.uuid = UNHEX(REPLACE(?, '-', ''))
    |sql}
    |> select_filter_sql
    |> Caqti_type.string ->! Repo_entity.t
  ;;

  let find pool id =
    let open Lwt.Infix in
    Utils.Database.find_opt
      (Pool_database.Label.value pool)
      find_request
      (id |> Pool_common.Id.value)
    >|= CCOption.to_result Pool_common.Message.(NotFound Field.Filter)
  ;;

  let component_base_query =
    {sql|
    LEFT JOIN pool_experiments
    ON pool_filter.uuid = pool_experiments.filter_uuid
    WHERE pool_experiments.filter_uuid IS NULL
   |sql}
  ;;

  let find_component_request =
    let open Caqti_request.Infix in
    Format.asprintf
      "%s AND pool_filter.uuid = UNHEX(REPLACE(?, '-', ''))"
      component_base_query
    |> select_filter_sql
    |> Caqti_type.string ->! Repo_entity.t
  ;;

  let find_component pool id =
    let open Lwt.Infix in
    Utils.Database.find_opt
      (Pool_database.Label.value pool)
      find_component_request
      (id |> Pool_common.Id.value)
    >|= CCOption.to_result Pool_common.Message.(NotFound Field.Filter)
  ;;

  let find_all_components_request =
    let open Caqti_request.Infix in
    component_base_query
    |> select_filter_sql
    |> Caqti_type.unit ->* Repo_entity.t
  ;;

  let find_all_components pool =
    Utils.Database.collect
      (Pool_database.Label.value pool)
      find_all_components_request
  ;;

  let insert_sql =
    {sql|
      INSERT INTO pool_filter (
        uuid,
        filter
      ) VALUES (
        UNHEX(REPLACE(?, '-', '')),
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

  let update_request =
    let open Caqti_request.Infix in
    {sql|
        UPDATE
          pool_filter
        SET
          filter = $2
        WHERE
          uuid = UNHEX(REPLACE($1, '-', ''))
      |sql}
    |> Repo_entity.Write.t ->. Caqti_type.unit
  ;;

  let update pool =
    Utils.Database.exec (Pool_database.Label.value pool) update_request
  ;;
end

let find = Sql.find
let find_all_components = Sql.find_all_components
let find_component = Sql.find_component
let insert = Sql.insert
let update = Sql.update
