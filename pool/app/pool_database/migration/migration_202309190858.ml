let add_reminder_manually_sent_timestamp =
  Sihl.Database.Migration.create_step
    ~label:"add reminder manually sent timestamp"
    {sql|
      ALTER TABLE pool_assignments
        ADD COLUMN reminder_manually_last_sent_at timestamp NULL AFTER external_data_id
    |sql}
;;

let migration () =
  Sihl.Database.Migration.(
    empty "202309190858" |> add_step add_reminder_manually_sent_timestamp)
;;
