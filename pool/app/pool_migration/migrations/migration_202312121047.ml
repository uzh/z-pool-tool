let create_experiment_language_column =
  Database.Migration.create_step
    ~label:"create experiment language column"
    {sql|
      ALTER TABLE pool_experiments
        ADD COLUMN language varchar(128) DEFAULT NULL AFTER description
    |sql}
;;

let migration () =
  Sihl.Database.Migration.(
    empty "202312121047" |> add_step create_experiment_language_column)
;;
