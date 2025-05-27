let drop_pool_tenant_gtx_columns =
  Database.Migration.Step.create
    ~label:"drop pool_tenant gtx columns"
    [%string
      {sql|
        ALTER TABLE pool_tenant
          DROP COLUMN gtx_api_key,
          DROP COLUMN gtx_sender
      |sql}]
;;

let migration () =
  Database.Migration.(empty "202504141225" |> add_step drop_pool_tenant_gtx_columns)
;;
