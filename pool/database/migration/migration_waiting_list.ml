let create_waiting_list_table =
  Sihl.Database.Migration.create_step
    ~label:"create waiting list table"
    {sql|
      CREATE TABLE IF NOT EXISTS pool_waiting_list (
        `id` bigint(20) unsigned NOT NULL AUTO_INCREMENT,
        `uuid` binary(16) NOT NULL,
        `subject_id` bigint(20) NOT NULL,
        `experiment_id` bigint(20) NOT NULL,
        `created_at` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP,
        `updated_at` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
      PRIMARY KEY (id),
      UNIQUE KEY `unique_uuid` (`uuid`)
      ) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci
    |sql}
;;

let migration () =
  Sihl.Database.Migration.(
    empty "waiting_list" |> add_step create_waiting_list_table)
;;
