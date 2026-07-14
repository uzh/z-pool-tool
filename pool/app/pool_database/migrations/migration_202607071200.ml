let add_cell_phone_verified_at_to_promoted =
  Database.Migration.Step.create
    ~label:"add cell_phone_verified_at to pool_contacts_promoted"
    {sql|
      ALTER TABLE pool_contacts_promoted
        ADD COLUMN cell_phone_verified_at DATETIME NULL DEFAULT NULL
        AFTER cell_phone
    |sql}
;;

let migration () =
  Database.Migration.(
    empty "202607071200" |> add_step add_cell_phone_verified_at_to_promoted)
;;
