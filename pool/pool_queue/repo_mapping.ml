open Utils.Lwt_result.Infix
open Entity_mapping
module Dynparam = Database.Dynparam

let sql_write_columns = [ "queue_uuid"; "entity_uuid" ]

let insert_request { entity; _ } =
  let open Caqti_request.Infix in
  let table, column = Entity.History.model_sql entity in
  [%string
    {sql|
      INSERT INTO %{table} (
        queue_uuid,
        %{column}
      ) VALUES (
        %{Entity.Id.sql_value_fragment "$1"},
        %{Entity.Id.sql_value_fragment "$2"}
      )
    |sql}]
  |> Repo_entity.Mapping.t ->. Caqti_type.unit
;;

let insert pool t = Database.exec pool (insert_request t) t
let insert_all label = Lwt_list.iter_s (insert label)

let find_all_by_queue_request =
  let open Caqti_request.Infix in
  [%string
    {sql|
      SELECT %{Entity.Id.sql_select_fragment ~field:"entity_uuid"}
      FROM pool_queue_job_mapping
      WHERE queue_uuid = %{Entity.Id.sql_value_fragment "$1"}
    |sql}]
  |> Repo_entity.Id.t ->? Pool_common.Repo.Id.t
;;

let duplicate_for_new_job label _ =
  Database.query label (fun connection ->
    let module Connection = (val connection : Caqti_lwt.CONNECTION) in
    let* () = Connection.start () in
    Lwt.catch
      (fun () ->
         (* let%lwt mappings =
            Connection.collect_list find_all_by_queue_request queue_uuid
            >|- Caqti_error.show
            ||> CCResult.get_or_failwith
            ||> CCList.map (Repo_entity.Mapping.create queue_uuid)

            let* () =
            Connection.populate
            ~table:"pool_queue_jobs_mapping"
            ~columns:sql_write_columns
            Repo_entity.Mapping.t
            (mappings |> CCList.map populatable |> to_stream)
            |> Lwt.map Caqti_error.uncongested
            in *)
         Connection.commit ())
      (fun exn ->
         Logs.err (fun m ->
           m "Job mapping duplication failed: %s" (Printexc.to_string exn));
         Connection.rollback ()))
;;

let find_instances_by_entity queue_table ?query pool (model, entity_uuid) =
  let join_table, join_column = Entity.History.model_sql model in
  let where =
    let open Pool_common.Repo in
    ( [%string {sql| %{join_column} = %{Pool_common.Id.sql_value_fragment "?"} |sql}]
    , Dynparam.(empty |> add Id.t entity_uuid) )
  in
  let joins = [%string {sql| JOIN %{join_table} M ON M.queue_uuid = uuid |sql}] in
  Query.collect_and_count
    pool
    query
    ~select:(Repo.find_all_request queue_table ~joins)
    ~where
    Repo_entity.Instance.t
;;

let find_related_request model =
  let table, column = Entity.History.model_sql model in
  let open Caqti_request.Infix in
  [%string
    {sql|
      SELECT %{Pool_common.Id.sql_select_fragment ~field:column}
      FROM %{table}
      WHERE queue_uuid = %{Entity.Id.sql_value_fragment "?"}
    |sql}]
  |> Repo_entity.Id.t ->? Pool_common.Repo.Id.t
;;

let find_related pool { Entity.Instance.id; _ } entity =
  Database.find_opt pool (find_related_request entity) id
;;
