let add_unique_database_label_constraint =
  Database.Migration.Step.create
    ~label:"add unique database_label constraint"
    {sql|
      ALTER TABLE pool_tenant
        ADD CONSTRAINT unique_database_label UNIQUE (database_label);
  |sql}
;;

let migration () =
  Sihl.Database.Migration.(
    empty "202403201132" |> add_step add_unique_database_label_constraint)
;;
