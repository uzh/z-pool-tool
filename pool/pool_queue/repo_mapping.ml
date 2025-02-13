open CCFun.Infix
open Utils.Lwt_result.Infix
module Dynparam = Database.Dynparam

let sql_select_columns =
  Pool_common.Id.sql_select_fragment ~field:"pool_queue_jobs_mapping.entity_uuid"
  :: Repo.sql_select_columns None
;;

let sql_write_columns = [ "queue_uuid"; "entity_uuid" ]

let insert_request =
  let open Caqti_request.Infix in
  [%string
    {sql|
      INSERT INTO pool_queue_jobs_mapping (
        queue_uuid,
        entity_uuid
      ) VALUES (
        %{Entity.Id.sql_value_fragment "$1"},
        %{Entity.Id.sql_value_fragment "$2"}
      )
    |sql}]
  |> Repo_entity.Mapping.Write.t ->. Caqti_type.unit
;;

let insert pool t = Database.exec pool insert_request t

let populatable =
  let open Entity_mapping.Write in
  fun { queue_uuid; entity_uuid; _ } ->
    { queue_uuid = Entity.Id.(Repo.to_bytes of_string value queue_uuid)
    ; entity_uuid = Pool_common.Id.(Repo.to_bytes of_string value entity_uuid)
    }
;;

let to_stream = CCList.rev %> Caqti_lwt.Stream.of_list

let insert_all label mappings =
  Database.query label (fun connection ->
    let module Connection = (val connection : Caqti_lwt.CONNECTION) in
    Connection.populate
      ~table:"pool_queue_jobs_mapping"
      ~columns:sql_write_columns
      Repo_entity.Mapping.Write.t
      (mappings |> CCList.map (Entity_mapping.to_write %> populatable) |> to_stream)
    |> Lwt.map Caqti_error.uncongested)
;;

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

let duplicate_for_new_job label queue_uuid =
  Database.query label (fun connection ->
    let module Connection = (val connection : Caqti_lwt.CONNECTION) in
    let* () = Connection.start () in
    Lwt.catch
      (fun () ->
         let%lwt mappings =
           Connection.collect_list find_all_by_queue_request queue_uuid
           >|- Caqti_error.show
           ||> CCResult.get_or_failwith
           ||> CCList.map (Repo_entity.Mapping.Write.create queue_uuid)
         in
         let* () =
           Connection.populate
             ~table:"pool_queue_jobs_mapping"
             ~columns:sql_write_columns
             Repo_entity.Mapping.Write.t
             (mappings |> CCList.map populatable |> to_stream)
           |> Lwt.map Caqti_error.uncongested
         in
         Connection.commit ())
      (fun exn ->
         Logs.err (fun m ->
           m "Job mapping duplication failed: %s" (Printexc.to_string exn));
         Connection.rollback ()))
;;

let find_instances_by_entity queue_table ?query pool entity_uuid =
  let joins =
    "JOIN pool_queue_jobs_mapping ON pool_queue_jobs_mapping.queue_uuid = uuid"
  in
  let select = Repo.find_all_request queue_table ~joins in
  let where =
    [%string {sql| entity_uuid = %{Pool_common.Id.sql_value_fragment "?"} |sql}]
  in
  let dyn = Dynparam.(empty |> add Pool_common.Repo.Id.t entity_uuid) in
  Query.collect_and_count pool query ~select ~where ~dyn Repo_entity.Instance.t
;;

let find_related_request entity =
  let joins =
    match entity with
    | `Contact ->
      {sql| INNER JOIN pool_contacts ON entity_uuid = pool_contacts.user_uuid |sql}
    | `Experiment ->
      {sql| INNER JOIN pool_experiments ON entity_uuid = pool_experiments.uuid |sql}
  in
  let open Caqti_request.Infix in
  [%string
    {sql|
      SELECT %{Pool_common.Id.sql_select_fragment ~field:"entity_uuid"}
      FROM pool_queue_jobs_mapping
      %{joins}
      WHERE queue_uuid = %{Entity.Id.sql_value_fragment "?"}
    |sql}]
  |> Repo_entity.Id.t ->? Pool_common.Repo.Id.t
;;

let find_related pool { Entity.Instance.id; _ } entity =
  Database.find_opt pool (find_related_request entity) id
;;
