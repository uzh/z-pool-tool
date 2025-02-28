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

let duplicate_mapping_request model =
  let open Caqti_request.Infix in
  let table, column = Entity.History.model_sql model in
  [%string
    {sql|
      INSERT INTO %{table} (queue_uuid, %{column})
      SELECT UNHEX(REPLACE(?, '-', '')), %{column}
      FROM %{table}
      WHERE queue_uuid = UNHEX(REPLACE(?, '-', ''));
    |sql}]
  |> Caqti_type.(t2 Repo_entity.Id.t Repo_entity.Id.t ->. unit)
;;

let duplicate_for_new_job pool { Entity.Instance.id; clone_of; _ } =
  match clone_of with
  | None -> Lwt.return_unit
  | Some clone_of ->
    Entity.History.all_models
    |> Lwt_list.iter_s (fun model ->
      Database.exec pool (duplicate_mapping_request model) (id, clone_of))
;;

let find_instances_by_entity queue_table ?query pool (model, entity_uuid) =
  let join_table, join_column = Entity.History.model_sql model in
  let where, dyn =
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
    ~dyn
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
