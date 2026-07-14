let add_duplicates_check_due_at_to_contacts =
  Database.Migration.Step.create
    ~label:"add duplicates check due at column to contacts"
    {sql|
      ALTER TABLE pool_contacts
        ADD COLUMN duplicates_check_due_at timestamp NULL AFTER duplicates_last_checked
    |sql}
;;

let add_duplicates_last_checked_index =
  Database.Migration.Step.create
    ~label:"add index on contacts duplicates last checked"
    {sql|
      CREATE INDEX pool_contacts_duplicates_last_checked_index
        ON pool_contacts (disabled, duplicates_last_checked)
    |sql}
;;

let add_duplicates_check_due_at_index =
  Database.Migration.Step.create
    ~label:"add index on contacts duplicates check due at"
    {sql|
      CREATE INDEX pool_contacts_duplicates_check_due_at_index
        ON pool_contacts (disabled, duplicates_check_due_at)
    |sql}
;;

let add_user_name_index =
  Database.Migration.Step.create
    ~label:"add index on user name"
    {sql|
      CREATE INDEX user_users_name_index ON user_users (name)
    |sql}
;;

let add_user_given_name_index =
  Database.Migration.Step.create
    ~label:"add index on user given name"
    {sql|
      CREATE INDEX user_users_given_name_index ON user_users (given_name)
    |sql}
;;

let add_cell_phone_index =
  Database.Migration.Step.create
    ~label:"add index on contacts cell phone"
    {sql|
      CREATE INDEX pool_contacts_cell_phone_index ON pool_contacts (cell_phone)
    |sql}
;;

let add_custom_field_answers_value_index =
  Database.Migration.Step.create
    ~label:"add index on custom field answers value"
    {sql|
      CREATE INDEX pool_custom_field_answers_value_index
        ON pool_custom_field_answers (custom_field_uuid, value(64))
    |sql}
;;

let add_custom_field_answers_admin_value_index =
  Database.Migration.Step.create
    ~label:"add index on custom field answers admin value"
    {sql|
      CREATE INDEX pool_custom_field_answers_admin_value_index
        ON pool_custom_field_answers (custom_field_uuid, admin_value(64))
    |sql}
;;

let migration () =
  Database.Migration.(
    empty "202607071000"
    |> add_step add_duplicates_check_due_at_to_contacts
    |> add_step add_duplicates_last_checked_index
    |> add_step add_duplicates_check_due_at_index
    |> add_step add_user_name_index
    |> add_step add_user_given_name_index
    |> add_step add_cell_phone_index
    |> add_step add_custom_field_answers_value_index
    |> add_step add_custom_field_answers_admin_value_index)
;;
