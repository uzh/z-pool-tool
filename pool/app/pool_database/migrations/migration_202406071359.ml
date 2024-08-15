let add_gtx_sender_column =
  Database.Migration.Step.create
    ~label:"add text gtx_sender to pool_tenants"
    {sql|
      ALTER TABLE pool_tenant
        ADD COLUMN gtx_sender VARCHAR(11) AFTER gtx_api_key
    |sql}
;;

let fill_gtx_sender_column_with_title =
  Database.Migration.Step.create
    ~label:"autofill gtx sender"
    {sql|
      UPDATE
        pool_tenant
      SET
        gtx_sender = LEFT(title, 11)
    |sql}
;;

let add_not_null_constraint =
  Database.Migration.Step.create
    ~label:"add not null constraints to gtx_sender of pool_tenant"
    {sql|
      ALTER TABLE pool_tenant
        MODIFY COLUMN gtx_sender VARCHAR(11) NOT NULL
    |sql}
;;

let migration () =
  Database.Migration.(
    empty "202406071359"
    |> add_step add_gtx_sender_column
    |> add_step fill_gtx_sender_column_with_title
    |> add_step add_not_null_constraint)
;;
