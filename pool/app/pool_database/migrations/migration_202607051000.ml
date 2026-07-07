let add_queue_jobs_name_last_error_at_index =
  Database.Migration.Step.create
    ~label:"add name last_error_at index to queue jobs"
    {sql|
      CREATE INDEX index_name_last_error_at ON pool_queue_jobs (name, last_error_at)
    |sql}
;;

let add_queue_jobs_history_name_last_error_at_index =
  Database.Migration.Step.create
    ~label:"add name last_error_at index to queue jobs history"
    {sql|
      CREATE INDEX index_name_last_error_at ON pool_queue_jobs_history (name, last_error_at)
    |sql}
;;

let migration () =
  Database.Migration.(
    empty "202607051000"
    |> add_step add_queue_jobs_name_last_error_at_index
    |> add_step add_queue_jobs_history_name_last_error_at_index)
;;
