let add_contact_person_fk_to_experiments =
  Sihl.Database.Migration.create_step
    ~label:"add contact admin fk to experiments"
    {sql|
      ALTER TABLE pool_experiments
        ADD COLUMN contact_person_uuid binary(16) AFTER filter_uuid
    |sql}
;;

let add_fk_contraint_to_contact_person_uuid =
  Sihl.Database.Migration.create_step
    ~label:"add contraint to contact_person_uuid"
    {sql|
      ALTER TABLE pool_experiments
        ADD CONSTRAINT fk_pool_experiments_admins
        FOREIGN KEY (contact_person_uuid) REFERENCES user_users(uuid)
    |sql}
;;

(* Should I add a constraint?
   https://stackoverflow.com/questions/3228161/could-i-make-a-column-in-a-table-only-allows-one-true-value-and-all-other-rows *)
let add_default_flag_to_smtp_accounts =
  Sihl.Database.Migration.create_step
    ~label:"add default flag to smtp accounts"
    {sql|
      ALTER TABLE pool_smtp
        ADD COLUMN default_account boolean DEFAULT true AFTER protocol
    |sql}
;;

let migration () =
  Sihl.Database.Migration.(
    empty "202307121722"
    |> add_step add_contact_person_fk_to_experiments
    |> add_step add_fk_contraint_to_contact_person_uuid
    |> add_step add_default_flag_to_smtp_accounts)
;;
