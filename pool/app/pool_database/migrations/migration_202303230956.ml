let add_uuid_columns_to_invitations_table =
  Database.Migration.Step.create
    ~label:"add uuid columns to invitations table"
    {sql|
      ALTER TABLE pool_invitations
        ADD COLUMN experiment_uuid binary(16) AFTER experiment_id,
        ADD COLUMN contact_uuid binary(16) AFTER contact_id
    |sql}
;;

let set_uuid_columns_on_invitations_table =
  Database.Migration.Step.create
    ~label:"set uuid columns on invitations table"
    {sql|
      UPDATE pool_invitations SET
        experiment_uuid = (SELECT uuid FROM pool_experiments WHERE pool_experiments.id = pool_invitations.experiment_id),
        contact_uuid = (SELECT user_uuid FROM pool_contacts WHERE pool_contacts.id = pool_invitations.contact_id)
    |sql}
;;

let add_constraint_to_invitations_table =
  Database.Migration.Step.create
    ~label:"add constraint to invitations table"
    {sql|
      ALTER TABLE pool_invitations
        ADD CONSTRAINT unique_contact_experiment_combination UNIQUE (experiment_uuid, contact_uuid)
    |sql}
;;

let make_invitation_uuids_not_nullable =
  Database.Migration.Step.create
    ~label:"make invitation uuids not nullable"
    {sql|
      ALTER TABLE pool_invitations
        MODIFY COLUMN `experiment_uuid` binary(16) NOT NULL,
        MODIFY COLUMN `contact_uuid` binary(16) NOT NULL
    |sql}
;;

let remove_id_columns_from_invitations =
  Database.Migration.Step.create
    ~label:"remove id columns from invitations"
    {sql|
      ALTER TABLE pool_invitations
        DROP COLUMN experiment_id,
        DROP COLUMN contact_id
    |sql}
;;

let add_uuid_column_to_mailings =
  Database.Migration.Step.create
    ~label:"add uuid column to mailings"
    {sql|
      ALTER TABLE pool_mailing
        ADD COLUMN experiment_uuid binary(16) AFTER experiment_id
    |sql}
;;

let set_uuid_column_on_mailing_table =
  Database.Migration.Step.create
    ~label:"set uuid column on mailing table"
    {sql|
      UPDATE pool_mailing SET
        experiment_uuid = (SELECT uuid FROM pool_experiments WHERE pool_experiments.id = pool_mailing.experiment_id)
    |sql}
;;

let make_mailing_uuid_foreignkey_not_nullable =
  Database.Migration.Step.create
    ~label:"make mailing uuid foreignkey not nullable"
    {sql|
      ALTER TABLE pool_mailing
        MODIFY COLUMN `experiment_uuid` binary(16) NOT NULL
    |sql}
;;

let drop_id_foreignkey_column_from_mailings =
  Database.Migration.Step.create
    ~label:"drop id foreignkey column from mailings"
    {sql|
      ALTER TABLE pool_mailing
        DROP COLUMN experiment_id
    |sql}
;;

let migration () =
  Sihl.Database.Migration.(
    empty "202303230956"
    |> add_step add_uuid_columns_to_invitations_table
    |> add_step set_uuid_columns_on_invitations_table
    |> add_step add_constraint_to_invitations_table
    |> add_step make_invitation_uuids_not_nullable
    |> add_step remove_id_columns_from_invitations
    |> add_step add_uuid_column_to_mailings
    |> add_step set_uuid_column_on_mailing_table
    |> add_step make_mailing_uuid_foreignkey_not_nullable
    |> add_step drop_id_foreignkey_column_from_mailings)
;;
