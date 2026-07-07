let add_usage_count_to_authentication =
  Database.Migration.Step.create
    ~label:"add usage_count to pool_authentication"
    {sql|
      ALTER TABLE pool_authentication
        MODIFY COLUMN valid_until TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP(),
        ADD COLUMN usage_count TINYINT UNSIGNED NOT NULL AFTER token
    |sql}
;;

(* This migration is executed on root and tenant databases *)
let migration () =
  Database.Migration.(empty "202607031200" |> add_step add_usage_count_to_authentication)
;;
