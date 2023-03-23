let use_uuids_as_foreign_keys_in_invitations_table =
  Sihl.Database.Migration.create_step
    ~label:"replace ids with uuids as foreignkeys in invitations table"
    {sql|
      BEGIN NOT ATOMIC
        ALTER TABLE pool_invitations
          ADD COLUMN experiment_uuid binary(16) AFTER experiment_id,
          ADD COLUMN contact_uuid binary(16) AFTER contact_id;

        UPDATE pool_invitations SET
          experiment_uuid = (SELECT uuid FROM pool_experiments WHERE pool_experiments.id = pool_invitations.experiment_id),
          contact_uuid = (SELECT user_uuid FROM pool_contacts WHERE pool_contacts.id = pool_invitations.contact_id);

        ALTER TABLE pool_invitations
          ADD CONSTRAINT unique_contact_experiment_combination UNIQUE (experiment_uuid, contact_uuid);

        ALTER TABLE pool_invitations
          MODIFY COLUMN `experiment_uuid` binary(16) NOT NULL,
          MODIFY COLUMN `contact_uuid` binary(16) NOT NULL;

        ALTER TABLE pool_invitations
          DROP COLUMN experiment_id,
          DROP COLUMN contact_id;
      END;
    |sql}
;;

let migration () =
  Sihl.Database.Migration.(
    empty "202303230956"
    |> add_step use_uuids_as_foreign_keys_in_invitations_table)
;;
