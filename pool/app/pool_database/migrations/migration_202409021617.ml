let add_database_label =
  Database.Migration.Step.create
    ~label:"add database label to pool_schedule"
    {sql|
      ALTER TABLE pool_schedules
        ADD COLUMN database_label varchar(255) AFTER label
    |sql}
;;

let migration () =
  Database.Migration.(empty "202409021617" |> add_step add_database_label)
;;
