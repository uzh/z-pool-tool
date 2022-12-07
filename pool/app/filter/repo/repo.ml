module Database = Pool_database
module Dynparam = Utils.Database.Dynparam

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
          pool_filter.query,
          pool_filter.title,
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
    let open Utils.Lwt_result.Infix in
    Utils.Database.find_opt
      (Pool_database.Label.value pool)
      find_request
      (id |> Pool_common.Id.value)
    ||> CCOption.to_result Pool_common.Message.(NotFound Field.Filter)
  ;;

  let component_base_query =
    {sql|
    LEFT JOIN pool_experiments
    ON pool_filter.uuid = pool_experiments.filter_uuid
    WHERE pool_experiments.filter_uuid IS NULL
   |sql}
  ;;

  let find_template_request =
    let open Caqti_request.Infix in
    Format.asprintf
      "%s AND pool_filter.uuid = UNHEX(REPLACE(?, '-', ''))"
      component_base_query
    |> select_filter_sql
    |> Caqti_type.string ->! Repo_entity.t
  ;;

  let find_template pool id =
    let open Utils.Lwt_result.Infix in
    Utils.Database.find_opt
      (Pool_database.Label.value pool)
      find_template_request
      (id |> Pool_common.Id.value)
    ||> CCOption.to_result Pool_common.Message.(NotFound Field.Filter)
  ;;

  let find_all_templates_request =
    let open Caqti_request.Infix in
    component_base_query
    |> select_filter_sql
    |> Caqti_type.unit ->* Repo_entity.t
  ;;

  let find_all_templates pool =
    Utils.Database.collect
      (Pool_database.Label.value pool)
      find_all_templates_request
  ;;

  let find_multiple_request ids =
    Format.asprintf
      {sql|
        WHERE pool_filter.uuid IN ( %s )
      |sql}
      (CCList.mapi
         (fun i _ -> Format.asprintf "UNHEX(REPLACE($%n, '-', ''))" (i + 1))
         ids
      |> CCString.concat ",")
    |> select_filter_sql
  ;;

  let find_multiple_templates pool ids =
    if CCList.is_empty ids
    then Lwt.return []
    else
      let open Caqti_request.Infix in
      let dyn =
        CCList.fold_left
          (fun dyn id ->
            dyn |> Dynparam.add Caqti_type.string (id |> Pool_common.Id.value))
          Dynparam.empty
          ids
      in
      let (Dynparam.Pack (pt, pv)) = dyn in
      let request = find_multiple_request ids |> pt ->* Repo_entity.t in
      Utils.Database.collect (pool |> Pool_database.Label.value) request pv
  ;;

  let insert_sql =
    {sql|
      INSERT INTO pool_filter (
        uuid,
        query,
        title
      ) VALUES (
        UNHEX(REPLACE(?, '-', '')),
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

  let update_request =
    let open Caqti_request.Infix in
    {sql|
        UPDATE
          pool_filter
        SET
          query = $2,
          title = $3
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
let find_all_templates = Sql.find_all_templates
let find_template = Sql.find_template
let find_multiple_templates = Sql.find_multiple_templates
let insert = Sql.insert
let update = Sql.update
