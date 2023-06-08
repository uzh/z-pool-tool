let rename_phone_number_to_cell_phone =
  Sihl.Database.Migration.create_step
    ~label:"rename phone number to cell phone"
    {sql|
      ALTER TABLE pool_contacts
        RENAME COLUMN phone_number TO cell_phone
    |sql}
;;

let rename_phone_number_to_cell_phone_in_verifiaction_table =
  Sihl.Database.Migration.create_step
    ~label:"rename phone number to cell phone in verification table"
    {sql|
      ALTER TABLE pool_phone_number_verifications
        RENAME COLUMN phone_number TO cell_phone
    |sql}
;;

let rename_verification_table =
  Sihl.Database.Migration.create_step
    ~label:"rename subjects to contacts table"
    {sql|
      ALTER TABLE pool_phone_number_verifications
        RENAME pool_cell_phone_verifications
    |sql}
;;

let migration () =
  Sihl.Database.Migration.(
    empty "202308071414"
    |> add_step rename_phone_number_to_cell_phone
    |> add_step rename_phone_number_to_cell_phone_in_verifiaction_table
    |> add_step rename_verification_table)
;;
