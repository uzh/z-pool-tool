let add_external_data_id_to_assignments =
  Database.Migration.create_step
    ~label:"add external data id to assignments"
    {sql|
      ALTER TABLE pool_assignments
       ADD COLUMN external_data_id varchar(255) AFTER marked_as_deleted
    |sql}
;;

let migration () =
  Sihl.Database.Migration.(
    empty "202308021016" |> add_step add_external_data_id_to_assignments)
;;
