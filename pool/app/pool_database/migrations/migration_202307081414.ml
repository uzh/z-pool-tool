let rename_phone_number_to_cell_phone =
  Database.Migration.Step.create
    ~label:"rename phone number to cell phone"
    {sql|
      ALTER TABLE pool_contacts
        RENAME COLUMN phone_number TO cell_phone
    |sql}
;;

let rename_phone_number_to_cell_phone_in_verifiaction_table =
  Database.Migration.Step.create
    ~label:"rename phone number to cell phone in verification table"
    {sql|
      ALTER TABLE pool_phone_number_verifications
        RENAME COLUMN phone_number TO cell_phone
    |sql}
;;

let rename_verification_table =
  Database.Migration.Step.create
    ~label:"rename subjects to contacts table"
    {sql|
      ALTER TABLE pool_phone_number_verifications
        RENAME pool_cell_phone_verifications
    |sql}
;;

let migration () =
  Sihl.Database.Migration.(
    empty "202307081414"
    |> add_step rename_phone_number_to_cell_phone
    |> add_step rename_phone_number_to_cell_phone_in_verifiaction_table
    |> add_step rename_verification_table)
;;
