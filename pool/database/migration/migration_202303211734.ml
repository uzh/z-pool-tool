let use_uuids_as_foreign_keys_in_waitinglist_table =
  Sihl.Database.Migration.create_step
    ~label:"replace ids with uuids as foreignkeys in waiting_list table"
    {sql|
      BEGIN NOT ATOMIC
        ALTER TABLE pool_waiting_list
          ADD COLUMN contact_uuid binary(16) AFTER contact_id,
          ADD COLUMN experiment_uuid binary(16) AFTER experiment_id;

        UPDATE pool_waiting_list SET
          contact_uuid = (SELECT user_uuid FROM pool_contacts WHERE pool_contacts.id = pool_waiting_list.contact_id),
          experiment_uuid = (SELECT uuid FROM pool_experiments WHERE pool_experiments.id = pool_waiting_list.experiment_id);

        ALTER TABLE pool_waiting_list
          ADD CONSTRAINT unique_contact_experiment_combination UNIQUE (contact_uuid, experiment_uuid);

        ALTER TABLE pool_waiting_list
          MODIFY COLUMN `contact_uuid` binary(16) NOT NULL,
          MODIFY COLUMN `experiment_uuid` binary(16) NOT NULL;

        ALTER TABLE pool_waiting_list
          DROP COLUMN contact_id,
          DROP COLUMN experiment_id;
      END;
    |sql}
;;

let migration () =
  Sihl.Database.Migration.(
    empty "202303211734"
    |> add_step use_uuids_as_foreign_keys_in_waitinglist_table)
;;
