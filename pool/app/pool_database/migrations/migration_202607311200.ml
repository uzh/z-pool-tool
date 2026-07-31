open Database

let extend_change_log_changes_column =
  Migration.Step.create
    ~label:"change pool_change_log.changes column type to longtext"
    {sql|
      ALTER TABLE pool_change_log
      MODIFY COLUMN `changes` LONGTEXT NOT NULL DEFAULT ''
    |sql}
;;

let migration () =
  Migration.(empty "202607311200" |> add_step extend_change_log_changes_column)
;;
