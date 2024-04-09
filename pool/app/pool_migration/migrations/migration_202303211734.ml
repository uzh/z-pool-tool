let add_uuid_foreignkey_columns_to_waiting_list =
  Database.Migration.create_step
    ~label:"add uuid foreignkey columns to waiting_list"
    {sql|
      ALTER TABLE pool_waiting_list
        ADD COLUMN contact_uuid binary(16) AFTER contact_id,
        ADD COLUMN experiment_uuid binary(16) AFTER experiment_id
    |sql}
;;

let set_uuid_foreignkey_columns_on_waiting_list =
  Database.Migration.create_step
    ~label:"set uuid foreignkey columns on waiting_list"
    {sql|
      UPDATE pool_waiting_list SET
        contact_uuid = (SELECT user_uuid FROM pool_contacts WHERE pool_contacts.id = pool_waiting_list.contact_id),
        experiment_uuid = (SELECT uuid FROM pool_experiments WHERE pool_experiments.id = pool_waiting_list.experiment_id)
    |sql}
;;

let add_unique_contraint_to_waiting_list =
  Database.Migration.create_step
    ~label:"add unique contraint to waiting_list"
    {sql|
      ALTER TABLE pool_waiting_list
        ADD CONSTRAINT unique_contact_experiment_combination UNIQUE (contact_uuid, experiment_uuid)
    |sql}
;;

let make_waiting_list_foreignkeys_not_nullable =
  Database.Migration.create_step
    ~label:"add unique contraint to waiting_list"
    {sql|
      ALTER TABLE pool_waiting_list
        MODIFY COLUMN `contact_uuid` binary(16) NOT NULL,
        MODIFY COLUMN `experiment_uuid` binary(16) NOT NULL
    |sql}
;;

let drop_waiting_list_id_foreignkey_columns =
  Database.Migration.create_step
    ~label:"add unique contraint to waiting_list"
    {sql|
      ALTER TABLE pool_waiting_list
        DROP COLUMN contact_id,
        DROP COLUMN experiment_id
    |sql}
;;

let migration () =
  Database.Migration.(
    empty "202303211734"
    |> add_step add_uuid_foreignkey_columns_to_waiting_list
    |> add_step set_uuid_foreignkey_columns_on_waiting_list
    |> add_step add_unique_contraint_to_waiting_list
    |> add_step make_waiting_list_foreignkeys_not_nullable
    |> add_step drop_waiting_list_id_foreignkey_columns)
;;
