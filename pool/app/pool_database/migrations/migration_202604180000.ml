let add_cell_phone_verified_at =
  Database.Migration.Step.create
    ~label:"add cell_phone_verified_at to pool_contacts"
    {sql|
      ALTER TABLE pool_contacts
        ADD COLUMN cell_phone_verified_at DATETIME NULL DEFAULT NULL
        AFTER cell_phone
    |sql}
;;

let backfill_cell_phone_verified_at =
  Database.Migration.Step.create
    ~label:"backfill cell_phone_verified_at for existing pool_contacts"
    {sql|
      UPDATE pool_contacts
      SET cell_phone_verified_at = updated_at
      WHERE cell_phone IS NOT NULL
        AND cell_phone_verified_at IS NULL
    |sql}
;;

let migration () =
  Database.Migration.(
    empty "202604180000"
    |> add_step add_cell_phone_verified_at
    |> add_step backfill_cell_phone_verified_at)
;;
