let add_cell_phone_verified_at =
  Database.Migration.Step.create
    ~label:"add cell_phone_verified_at to pool_contacts"
    {sql|
      ALTER TABLE pool_contacts
        ADD COLUMN cell_phone_verified_at DATETIME NULL DEFAULT NULL
        AFTER cell_phone
    |sql}
;;

let migration () =
  Database.Migration.(empty "202604180000" |> add_step add_cell_phone_verified_at)
;;
