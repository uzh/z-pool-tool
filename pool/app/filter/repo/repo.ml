module Database = Pool_database
module Dynparam = Utils.Database.Dynparam
open Entity
open Repo_utils

let src = Logs.Src.create "filter.repo"

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
        AND pool_filter.title IS NOT NULL
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

  let delete_request =
    let open Caqti_request.Infix in
    {sql|
      DELETE FROM pool_filter
      WHERE uuid = UNHEX(REPLACE($1, '-', ''))
    |sql}
    |> Pool_common.Repo.Id.t ->. Caqti_type.unit
  ;;

  let delete pool =
    Utils.Database.exec (Pool_database.Label.value pool) delete_request
  ;;

  let find_templates_of_query tenant_db query =
    let open Utils.Lwt_result.Infix in
    let rec go queries ids templates =
      match queries with
      | [] -> templates |> Lwt.return
      | _ ->
        let new_ids = CCList.flat_map (search_templates []) queries in
        CCList.filter
          (fun id -> Stdlib.not (CCList.mem ~eq:Pool_common.Id.equal id ids))
          new_ids
        |> find_multiple_templates tenant_db
        >|> fun filter_list ->
        go
          (filter_list |> CCList.map (fun f -> f.query))
          (ids @ new_ids)
          (templates @ filter_list)
    in
    go [ query ] [] []
  ;;

  let find_filtered_request_sql ?limit where_fragment =
    let base = Contact.Repo.Sql.find_request_sql where_fragment in
    match limit with
    | None -> base
    | Some limit -> Format.asprintf "%s\nLIMIT %i" base limit
  ;;

  let find_filtered_contacts pool ?order_by ?limit experiment_id filter =
    let filter = filter |> CCOption.map (fun f -> f.Entity.query) in
    let open Utils.Lwt_result.Infix in
    let%lwt template_list =
      match filter with
      | None -> Lwt.return []
      | Some filter -> find_templates_of_query pool filter
    in
    filtered_params
      template_list
      ~group_by:"pool_contacts.user_uuid"
      ?order_by
      experiment_id
      filter
    |> Lwt_result.lift
    >>= fun (dyn, sql) ->
    let (Dynparam.Pack (pt, pv)) = dyn in
    let open Caqti_request.Infix in
    let request =
      sql
      |> where_prefix
      |> find_filtered_request_sql ?limit
      |> pt ->* Contact.Repo.Entity.t
    in
    let%lwt contacts =
      Utils.Database.collect (pool |> Pool_database.Label.value) request pv
    in
    Lwt_result.return contacts
  ;;

  let contact_matches_filter
    ?(default = false)
    pool
    experiment_id
    query
    (contact : Contact.t)
    =
    let open Utils.Lwt_result.Infix in
    let tags = Pool_database.Logger.Tags.create pool in
    let find_sql where_fragment =
      Format.asprintf
        {sql|
          SELECT 1
          FROM pool_contacts
            LEFT JOIN user_users
            ON pool_contacts.user_uuid = user_users.uuid
          WHERE
          %s
          AND pool_contacts.user_uuid = UNHEX(REPLACE(?, '-', ''))
        |sql}
        where_fragment
    in
    let%lwt template_list = find_templates_of_query pool query in
    filtered_params ~allow_invited:true template_list experiment_id (Some query)
    |> CCResult.map_err (fun err ->
      let () =
        Logs.info ~src (fun m ->
          m
            ~tags
            "%s\n%s"
            ([%show: Contact.t] contact)
            ([%show: Pool_common.Id.t] experiment_id))
      in
      Pool_common.Utils.with_log_error ~src ~level:Logs.Warning ~tags err)
    |> CCResult.get_or
         ~default:(Dynparam.empty, if default then "TRUE" else "FALSE")
    |> fun (dyn, sql) ->
    let dyn =
      Dynparam.(
        dyn |> add Caqti_type.string Contact.(contact |> id |> Id.value))
    in
    let (Dynparam.Pack (pt, pv)) = dyn in
    let open Caqti_request.Infix in
    let request = sql |> find_sql |> pt ->? Caqti_type.int in
    let%lwt contacts =
      Utils.Database.find_opt (pool |> Pool_database.Label.value) request pv
      ||> CCOption.map_or ~default (CCInt.equal 1)
    in
    Lwt.return contacts
  ;;

  let count_filtered_request_sql where_fragment =
    let select =
      {sql|
        SELECT COUNT(*)
        FROM pool_contacts
          LEFT JOIN user_users
          ON pool_contacts.user_uuid = user_users.uuid
      |sql}
    in
    Format.asprintf "%s\n%s" select where_fragment
  ;;

  let count_filtered_contacts pool experiment_id query =
    let open Utils.Lwt_result.Infix in
    let%lwt template_list =
      match query with
      | None -> Lwt.return []
      | Some query -> find_templates_of_query pool query
    in
    filtered_params template_list experiment_id query
    |> Lwt_result.lift
    >>= fun (dyn, sql) ->
    let (Dynparam.Pack (pt, pv)) = dyn in
    let open Caqti_request.Infix in
    let request =
      sql |> where_prefix |> count_filtered_request_sql |> pt ->! Caqti_type.int
    in
    let%lwt count =
      Utils.Database.find_opt (pool |> Pool_database.Label.value) request pv
    in
    Lwt_result.return (count |> CCOption.value ~default:0)
  ;;

  let find_experiment_id_opt_request =
    let open Caqti_request.Infix in
    {sql|
      SELECT
        LOWER(CONCAT(
          SUBSTR(HEX(pool_experiments.uuid), 1, 8), '-',
          SUBSTR(HEX(pool_experiments.uuid), 9, 4), '-',
          SUBSTR(HEX(pool_experiments.uuid), 13, 4), '-',
          SUBSTR(HEX(pool_experiments.uuid), 17, 4), '-',
          SUBSTR(HEX(pool_experiments.uuid), 21)
        ))
      FROM pool_experiments
        LEFT JOIN pool_filter
        ON pool_experiments.filter_uuid = pool_filter.uuid
      WHERE
        pool_filter.uuid = UNHEX(REPLACE(?, '-', ''))
    |sql}
    |> Repo_entity.Id.t ->? Pool_common.Repo.Id.t
  ;;

  let find_experiment_id_opt pool id =
    Utils.Database.find_opt
      (Pool_database.Label.value pool)
      find_experiment_id_opt_request
      id
  ;;
end

let find = Sql.find
let find_all_templates = Sql.find_all_templates
let find_template = Sql.find_template
let find_multiple_templates = Sql.find_multiple_templates
let find_templates_of_query = Sql.find_templates_of_query
let find_filtered_contacts = Sql.find_filtered_contacts
let find_experiment_id_opt = Sql.find_experiment_id_opt
let count_filtered_contacts = Sql.count_filtered_contacts
let contact_matches_filter = Sql.contact_matches_filter
let insert = Sql.insert
let update = Sql.update
let delete = Sql.delete
