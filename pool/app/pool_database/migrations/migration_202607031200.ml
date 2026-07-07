let remove_automagic_timestamp_on_update_trigger =
  Database.Migration.Step.create
    ~label:
      "remove mariadbs automagic timestamp on update trigger for token valid_until column"
    {sql|
      ALTER TABLE pool_authentication
      MODIFY COLUMN valid_until TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP()
    |sql}
;;

let add_usage_count_to_authentication =
  Database.Migration.Step.create
    ~label:"add usage_count to pool_authentication"
    {sql|
      ALTER TABLE pool_authentication
      ADD COLUMN usage_count TINYINT UNSIGNED NOT NULL DEFAULT 0 AFTER token
    |sql}
;;

let migration () =
  Database.Migration.(
    empty "202607031200"
    |> add_step remove_automagic_timestamp_on_update_trigger
    |> add_step add_usage_count_to_authentication)
;;
