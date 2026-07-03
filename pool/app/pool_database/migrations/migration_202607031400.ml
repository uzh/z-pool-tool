let add_smtp_bounces_count_to_admin =
  Database.Migration.Step.create
    ~label:"add smtp bounces count column to admin"
    {sql|
      ALTER TABLE pool_admins
        ADD COLUMN smtp_bounces_count SMALLINT(3) DEFAULT 0 AFTER import_pending
    |sql}
;;

let migration () =
  Database.Migration.(empty "202607031400" |> add_step add_smtp_bounces_count_to_admin)
;;
