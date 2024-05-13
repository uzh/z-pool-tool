let add_gtx_api_key_to_tenant =
  Database.Migration.Step.create
    ~label:"add gtx api key to tenant"
    {sql|
    ALTER TABLE pool_tenant
      ADD COLUMN gtx_api_key varchar(255) AFTER database_label
    |sql}
;;

let migration () =
  Database.Migration.(
    empty "202305261314" |> add_step add_gtx_api_key_to_tenant)
;;
