include Entity

let insert = Repo.insert

let callback
  database_label
  { entity_uuids; message_template }
  (job_instance : Sihl_queue.instance)
  =
  entity_uuids
  |> Lwt_list.iter_s (fun entity_uuid ->
    create ?message_template ~entity_uuid job_instance |> insert database_label)
;;
