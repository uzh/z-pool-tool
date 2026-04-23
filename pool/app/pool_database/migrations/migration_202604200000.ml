let add_expires_at_to_cell_phone_verifications =
  Database.Migration.Step.create
    ~label:"add expires_at to pool_cell_phone_verifications"
    {sql|
      ALTER TABLE pool_cell_phone_verifications
        ADD COLUMN expires_at DATETIME NULL
        AFTER token
    |sql}
;;

let backfill_expires_at_for_cell_phone_verifications =
  Database.Migration.Step.create
    ~label:"backfill expires_at for pool_cell_phone_verifications"
    {sql|
      UPDATE pool_cell_phone_verifications
      SET expires_at = updated_at + INTERVAL 1 HOUR
      WHERE expires_at IS NULL
    |sql}
;;

let enforce_expires_at_on_cell_phone_verifications =
  Database.Migration.Step.create
    ~label:"enforce expires_at on pool_cell_phone_verifications"
    {sql|
      ALTER TABLE pool_cell_phone_verifications
        MODIFY COLUMN expires_at DATETIME NOT NULL DEFAULT (NOW() + INTERVAL 1 HOUR)
    |sql}
;;

let migration () =
  Database.Migration.(
    empty "202604200000"
    |> add_step add_expires_at_to_cell_phone_verifications
    |> add_step backfill_expires_at_for_cell_phone_verifications
    |> add_step enforce_expires_at_on_cell_phone_verifications)
;;
