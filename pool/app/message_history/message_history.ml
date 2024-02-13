include Entity

let insert = Repo.insert

let create_from_queue_instance
  database_label
  { entity_uuids; message_template }
  (job_instance : Sihl_queue.instance)
  =
  entity_uuids
  |> Lwt_list.iter_s (fun entity_uuid ->
    create ?message_template ~entity_uuid job_instance |> insert database_label)
;;

let query_by_entity = Repo.query_by_entity

module Repo = struct
  module Entity = struct
    let sihl_queue_job_caqti = Repo_entity.sihl_queue_job_caqti
  end

  let sql_select_job_queue_columns = Repo.sql_select_job_queue_columns
end
