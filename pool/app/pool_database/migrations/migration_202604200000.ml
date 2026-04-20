let add_expires_at_to_cell_phone_verifications =
  Database.Migration.Step.create
    ~label:"add expires_at to pool_cell_phone_verifications"
    {sql|
      ALTER TABLE pool_cell_phone_verifications
        ADD COLUMN expires_at DATETIME NOT NULL DEFAULT (NOW() + INTERVAL 1 HOUR)
        AFTER token
    |sql}
;;

let migration () =
  Database.Migration.(
    empty "202604200000" |> add_step add_expires_at_to_cell_phone_verifications)
;;
