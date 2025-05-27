let add_contact_email_column =
  Database.Migration.Step.create
    ~label:"add contact email column"
    {sql|
      ALTER TABLE pool_experiments
        ADD COLUMN contact_email varchar(255) NULL AFTER contact_person_uuid
    |sql}
;;

let fill_contact_email_column =
  Database.Migration.Step.create
    ~label:"fill contact_email column"
    {sql|
    UPDATE
      pool_experiments
    SET
      contact_email = (SELECT email FROM user_users WHERE uuid = pool_experiments.contact_person_uuid)
    WHERE
      pool_experiments.contact_person_uuid IS NOT NULL;
    |sql}
;;

let drop_fk =
  Database.Migration.Step.create
    ~label:"drop fk_pool_experiments_admins"
    {sql|
    ALTER TABLE pool_experiments
      DROP CONSTRAINT IF EXISTS fk_pool_experiments_admins
    |sql}
;;

let remove_contact_uuid_column =
  Database.Migration.Step.create
    ~label:"remove contact_uuid column"
    {sql|
      ALTER TABLE pool_experiments
      DROP COLUMN contact_person_uuid
    |sql}
;;

let migration () =
  Database.Migration.(
    empty "202403281435"
    |> add_step add_contact_email_column
    |> add_step fill_contact_email_column
    |> add_step drop_fk
    |> add_step remove_contact_uuid_column)
;;
