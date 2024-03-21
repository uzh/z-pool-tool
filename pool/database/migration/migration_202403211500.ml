let add_worker_only_flag_to_system_events =
  Sihl.Database.Migration.create_step
    ~label:"add worker only flag to system_events"
    {sql|
      ALTER TABLE pool_system_events
        ADD COLUMN worker_only boolean DEFAULT 0 AFTER job
  |sql}
;;

let migration () =
  Sihl.Database.Migration.(
    empty "202403211500" |> add_step add_worker_only_flag_to_system_events)
;;
