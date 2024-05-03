let add_contact_person_fk_to_experiments =
  Database.Migration.Step.create
    ~label:"add contact admin fk to experiments"
    {sql|
      ALTER TABLE pool_experiments
        ADD COLUMN contact_person_uuid binary(16) AFTER filter_uuid
    |sql}
;;

let add_fk_contraint_to_contact_person_uuid =
  Database.Migration.Step.create
    ~label:"add contraint to contact_person_uuid"
    {sql|
      ALTER TABLE pool_experiments
        ADD CONSTRAINT fk_pool_experiments_admins
        FOREIGN KEY (contact_person_uuid) REFERENCES user_users(uuid)
    |sql}
;;

let add_default_flag_to_smtp_accounts =
  Database.Migration.Step.create
    ~label:"add default flag to smtp accounts"
    {sql|
      ALTER TABLE pool_smtp
        ADD COLUMN default_account boolean DEFAULT true AFTER protocol
    |sql}
;;

let add_smpt_fk_to_experiments =
  Database.Migration.Step.create
    ~label:"add contact smtp fk to experiments"
    {sql|
      ALTER TABLE pool_experiments
        ADD COLUMN smtp_auth_uuid binary(16) AFTER contact_person_uuid
    |sql}
;;

let add_fk_contraint_to_smtp_auth_uuid =
  Database.Migration.Step.create
    ~label:"add contraint to smtp_auth_uuid"
    {sql|
      ALTER TABLE pool_experiments
        ADD CONSTRAINT fk_pool_experiments_smtp_auth
        FOREIGN KEY (smtp_auth_uuid) REFERENCES pool_smtp(uuid)
    |sql}
;;

let migration () =
  Database.Migration.(
    empty "202307121722"
    |> add_step add_contact_person_fk_to_experiments
    |> add_step add_fk_contraint_to_contact_person_uuid
    |> add_step add_default_flag_to_smtp_accounts
    |> add_step add_smpt_fk_to_experiments
    |> add_step add_fk_contraint_to_smtp_auth_uuid)
;;

let migration_root () =
  Database.Migration.(
    empty "202306231722" |> add_step add_default_flag_to_smtp_accounts)
;;
