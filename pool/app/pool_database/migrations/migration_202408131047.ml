let add_email_logo_column =
  Database.Migration.Step.create
    ~label:"add uuid foreignkey columns to waiting_list"
    {sql|
      ALTER TABLE pool_tenant
        ADD COLUMN email_logo binary(16) AFTER icon
    |sql}
;;

let migration () =
  Database.Migration.(empty "2024008131047" |> add_step add_email_logo_column)
;;
