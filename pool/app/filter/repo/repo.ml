module Database = Database
module Dynparam = Database.Dynparam
open Entity
open Repo_utils

let src = Logs.Src.create "filter.repo"

let sql_select_columns =
  [ Id.sql_select_fragment ~field:"pool_filter.uuid"
  ; "pool_filter.query"
  ; "pool_filter.title"
  ; "pool_filter.created_at"
  ; "pool_filter.updated_at"
  ]
;;

module Sql = struct
  let find_request_sql ?(count = false) ?joins where_fragment =
    let columns =
      if count then "COUNT(*)" else sql_select_columns |> CCString.concat ", "
    in
    Format.asprintf
      {sql|SELECT %s FROM pool_filter %s %s|sql}
      columns
      (joins |> CCOption.value ~default:"")
      where_fragment
  ;;

  let find_request =
    let open Caqti_request.Infix in
    {sql|
      WHERE pool_filter.uuid = UNHEX(REPLACE(?, '-', ''))
    |sql}
    |> find_request_sql
    |> Caqti_type.string ->! Repo_entity.t
  ;;

  let find pool id =
    let open Utils.Lwt_result.Infix in
    Database.find_opt pool find_request (id |> Pool_common.Id.value)
    ||> CCOption.to_result Pool_message.(Error.NotFound Field.Filter)
  ;;

  let joins_experiment =
    {sql|
      LEFT JOIN pool_experiments
        ON pool_filter.uuid = pool_experiments.filter_uuid
    |sql}
  ;;

  let template_condition =
    "pool_experiments.filter_uuid IS NULL AND pool_filter.title IS NOT NULL"
  ;;

  let component_base_query =
    Format.asprintf "%s WHERE %s" joins_experiment template_condition
  ;;

  let find_template_request =
    let open Caqti_request.Infix in
    Format.asprintf
      "%s AND pool_filter.uuid = UNHEX(REPLACE(?, '-', ''))"
      component_base_query
    |> find_request_sql
    |> Caqti_type.string ->! Repo_entity.t
  ;;

  let find_template pool id =
    let open Utils.Lwt_result.Infix in
    Database.find_opt pool find_template_request (id |> Pool_common.Id.value)
    ||> CCOption.to_result Pool_message.(Error.NotFound Field.Filter)
  ;;

  let find_all_templates_request =
    let open Caqti_request.Infix in
    component_base_query
    |> find_request_sql
    |> Caqti_type.unit ->* Repo_entity.t
  ;;

  let find_all_templates pool = Database.collect pool find_all_templates_request

  let find_templates_by query pool =
    let where = template_condition, Dynparam.empty in
    Query.collect_and_count
      pool
      (Some query)
      ~select:(find_request_sql ~joins:joins_experiment)
      ~where
      Repo_entity.t
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
    |> find_request_sql
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
      Database.collect pool request pv
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

  let insert pool = Database.exec pool insert_request

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

  let update pool = Database.exec pool update_request

  let delete_request =
    let open Caqti_request.Infix in
    {sql|
      DELETE FROM pool_filter
      WHERE uuid = UNHEX(REPLACE($1, '-', ''))
    |sql}
    |> Pool_common.Repo.Id.t ->. Caqti_type.unit
  ;;

  let delete pool = Database.exec pool delete_request

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

  let drop_temp_table connection =
    let run_query q =
      let open Caqti_request.Infix in
      let (module Connection : Caqti_lwt.CONNECTION) = connection in
      let request = q |> Caqti_type.(unit ->. unit) in
      Connection.exec request ()
    in
    let queries =
      [ {sql| DROP TEMPORARY TABLE IF EXISTS tmp_participations; |sql}
      ; {sql| DROP TEMPORARY TABLE IF EXISTS tmp_invitations; |sql}
      ; {sql| DROP TEMPORARY TABLE IF EXISTS tmp_assignments; |sql}
      ]
    in
    Lwt_list.map_s run_query queries
    |> Lwt.map CCResult.flatten_l
    |> Lwt_result.map Utils.flat_unit
  ;;

  let find_participation_experiments_of_query query =
    let rec search ids query =
      let search_list ids =
        CCList.fold_left (fun ids predicate -> search ids predicate) ids
      in
      match query with
      | And lst | Or lst -> search_list ids lst
      | Not f -> search ids f
      | Template _ -> ids
      | Pred { Predicate.key; value; _ } ->
        let open Key in
        (match[@warning "-4"] key with
         | Hardcoded key ->
           if equal_hardcoded key Participation
           then (
             match value with
             | Lst values -> values @ ids
             | _ -> ids)
           else ids
         | _ -> ids)
    in
    search [] query
    |> CCList.filter_map (fun value ->
      match value with
      | Str id -> Some id
      | Bool _ | Date _ | Language _ | Nr _ | Option _ -> None)
  ;;

  let create_temporary_invitation_table query =
    let open Dynparam in
    let open Caqti_request.Infix in
    let create_request ids =
      Format.asprintf
        {sql|
        CREATE TEMPORARY TABLE tmp_invitations
        (INDEX contact_index (contact_uuid),
         INDEX experiment_index (experiment_uuid)
        )
        AS (
          SELECT
            pool_invitations.contact_uuid AS contact_uuid,
            pool_invitations.experiment_uuid AS experiment_uuid
          FROM pool_invitations
          WHERE experiment_uuid IN ( %s ))
        |sql}
        (CCList.mapi
           (fun i _ -> Format.asprintf "UNHEX(REPLACE($%n, '-', ''))" (i + 1))
           ids
         |> CCString.concat ",")
    in
    let open CCOption in
    let fnc connection =
      query
      >|= Repo_utils.find_experiments_by_key Key.Invitation
      |> function
      | None | Some [] -> Lwt_result.return ()
      | Some ids ->
        let (Pack (pt, pv)) =
          CCList.fold_left
            (fun dyn id -> dyn |> add Caqti_type.string id)
            empty
            ids
        in
        let (module Connection : Caqti_lwt.CONNECTION) = connection in
        let request = create_request ids |> pt ->. Caqti_type.unit in
        Connection.exec request pv
    in
    fnc
  ;;

  let create_temporary_assignments_table query =
    let open Dynparam in
    let open Caqti_request.Infix in
    let create_request ids =
      Format.asprintf
        {sql|
        CREATE TEMPORARY TABLE tmp_assignments
        (INDEX contact_index (contact_uuid),
         INDEX experiment_index (experiment_uuid)
        )
        AS (
          SELECT
            pool_assignments.contact_uuid AS contact_uuid,
            pool_sessions.experiment_uuid AS experiment_uuid
          FROM pool_assignments
            INNER JOIN pool_sessions ON pool_sessions.uuid = pool_assignments.session_uuid
          WHERE experiment_uuid IN ( %s ))
        |sql}
        (CCList.mapi
           (fun i _ -> Format.asprintf "UNHEX(REPLACE($%n, '-', ''))" (i + 1))
           ids
         |> CCString.concat ",")
    in
    let open CCOption in
    let fnc connection =
      query
      >|= Repo_utils.find_experiments_by_key Key.Assignment
      |> function
      | None | Some [] -> Lwt_result.return ()
      | Some ids ->
        let (Pack (pt, pv)) =
          CCList.fold_left
            (fun dyn id -> dyn |> add Caqti_type.string id)
            empty
            ids
        in
        let (module Connection : Caqti_lwt.CONNECTION) = connection in
        let request = create_request ids |> pt ->. Caqti_type.unit in
        Connection.exec request pv
    in
    fnc
  ;;

  let create_temporary_participation_table query =
    let open Dynparam in
    let open Caqti_request.Infix in
    let create_request ids =
      Format.asprintf
        {sql|
        CREATE TEMPORARY TABLE tmp_participations
        (INDEX contact_index (contact_uuid),
         INDEX experiment_index (experiment_uuid)
        )
        AS (
          SELECT
            pool_assignments.contact_uuid AS contact_uuid,
            pool_sessions.experiment_uuid AS experiment_uuid
          FROM
            pool_assignments
            INNER JOIN pool_sessions ON pool_sessions.uuid = pool_assignments.session_uuid
              AND pool_assignments.participated = 1
              AND pool_assignments.canceled_at IS NULL
              AND pool_sessions.experiment_uuid IN ( %s ))
        |sql}
        (CCList.mapi
           (fun i _ -> Format.asprintf "UNHEX(REPLACE($%n, '-', ''))" (i + 1))
           ids
         |> CCString.concat ",")
    in
    let open CCOption in
    let fnc connection =
      query
      >|= Repo_utils.find_experiments_by_key Key.Participation
      |> function
      | None | Some [] -> Lwt_result.return ()
      | Some ids ->
        let (Pack (pt, pv)) =
          CCList.fold_left
            (fun dyn id -> dyn |> add Caqti_type.string id)
            empty
            ids
        in
        let (module Connection : Caqti_lwt.CONNECTION) = connection in
        let request = create_request ids |> pt ->. Caqti_type.unit in
        Connection.exec request pv
    in
    fnc
  ;;

  let create_temp_tables filter =
    [ create_temporary_participation_table filter
    ; create_temporary_invitation_table filter
    ; create_temporary_assignments_table filter
    ]
  ;;

  let prepare_use_case_joins dyn =
    let open Dynparam in
    let invitation_join =
      {|LEFT JOIN pool_invitations ON pool_invitations.contact_uuid = pool_contacts.user_uuid
        AND pool_invitations.experiment_uuid = UNHEX(REPLACE(?, '-', ''))|}
    in
    function
    | MatchesFilter -> dyn, []
    | Matcher experiment_id ->
      let id = experiment_id |> Pool_common.Id.value in
      dyn |> prefix Caqti_type.string id, [ invitation_join ]
    | MatcherReset (experiment_id, _) ->
      let id = experiment_id |> Pool_common.Id.value in
      dyn |> prefix Caqti_type.string id, [ invitation_join ]
  ;;

  let find_filtered_request_sql ?limit use_case dyn where_fragment =
    let dyn, additional_joins = prepare_use_case_joins dyn use_case in
    let base = Contact.Repo.find_request_sql ~additional_joins where_fragment in
    match limit with
    | None -> dyn, base
    | Some limit -> dyn, Format.asprintf "%s\nLIMIT %i" base limit
  ;;

  let find_filtered_contacts pool ?order_by ?limit use_case filter =
    let filter = filter |> CCOption.map (fun f -> f.Entity.query) in
    let open Utils.Lwt_result.Infix in
    let%lwt template_list =
      match filter with
      | None -> Lwt.return []
      | Some filter -> find_templates_of_query pool filter
    in
    let order_by =
      match use_case with
      | MatcherReset _ when CCOption.is_none order_by ->
        Some "ORDER BY pool_invitations.created_at ASC"
      | MatchesFilter | Matcher _ | MatcherReset _ -> order_by
    in
    filtered_params
      use_case
      template_list
      ~group_by:"pool_contacts.user_uuid"
      ?order_by
      filter
    |> Lwt_result.lift
    >>= fun (dyn, sql) ->
    let open Caqti_request.Infix in
    let Dynparam.Pack (pt, pv), prepared_request =
      sql |> where_prefix |> find_filtered_request_sql ?limit use_case dyn
    in
    let request = prepared_request |> pt ->* Contact.Repo.t in
    let query connection =
      let (module Connection : Caqti_lwt.CONNECTION) = connection in
      Connection.collect_list request pv
    in
    Database.transaction
      pool
      ~setup:(drop_temp_table :: create_temp_tables filter)
      ~cleanup:[ drop_temp_table ]
      query
    ||> CCResult.return
  ;;

  let contact_matches_filter ?(default = false) pool query (contact : Contact.t)
    =
    let open Utils.Lwt_result.Infix in
    let tags = Database.Logger.Tags.create pool in
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
    filtered_params MatchesFilter template_list (Some query)
    |> CCResult.map_err
         (Pool_common.Utils.with_log_error ~src ~level:Logs.Warning ~tags)
    |> CCResult.get_or
         ~default:(Dynparam.empty, if default then "TRUE" else "FALSE")
    |> fun (dyn, sql) ->
    let (Dynparam.Pack (pt, pv)) =
      Dynparam.(dyn |> add Contact.Repo.Id.t Contact.(contact |> id))
    in
    let open Caqti_request.Infix in
    let request = sql |> find_sql |> pt ->? Caqti_type.int in
    let matches_filter_request connection =
      let (module Connection : Caqti_lwt.CONNECTION) = connection in
      Connection.find_opt request pv
    in
    Database.transaction
      pool
      ~setup:(drop_temp_table :: create_temp_tables (Some query))
      ~cleanup:[ drop_temp_table ]
      matches_filter_request
    ||> CCOption.map_or ~default (CCInt.equal 1)
  ;;

  let count_filtered_request_sql use_case dyn where_fragment =
    let dyn, joins = prepare_use_case_joins dyn use_case in
    let joins = CCString.concat "\n" joins in
    let select =
      {sql|
        SELECT COUNT(*)
        FROM pool_contacts
          LEFT JOIN user_users
          ON pool_contacts.user_uuid = user_users.uuid
      |sql}
    in
    dyn, Format.asprintf "%s\n%s\n%s" select joins where_fragment
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
    Database.find_opt pool find_experiment_id_opt_request id
  ;;

  let count_filtered_contacts ?include_invited pool use_case query =
    let open Utils.Lwt_result.Infix in
    let open Caqti_request.Infix in
    let open Dynparam in
    let%lwt template_list =
      match query with
      | None -> Lwt.return []
      | Some query -> find_templates_of_query pool query
    in
    filtered_params ?include_invited use_case template_list query
    |> Lwt_result.lift
    >>= fun (dyn, sql) ->
    let Pack (pt, pv), prepared_request =
      sql |> where_prefix |> count_filtered_request_sql use_case dyn
    in
    let request = prepared_request |> pt ->! Caqti_type.int in
    let filter = query in
    let query connection =
      let (module Connection : Caqti_lwt.CONNECTION) = connection in
      Connection.find_opt request pv
    in
    Database.transaction
      pool
      ~setup:(drop_temp_table :: create_temp_tables filter)
      ~cleanup:[ drop_temp_table ]
      query
    ||> CCOption.value ~default:0
    ||> CCResult.return
  ;;
end

let find = Sql.find
let find_all_templates = Sql.find_all_templates
let find_templates_by = Sql.find_templates_by
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
