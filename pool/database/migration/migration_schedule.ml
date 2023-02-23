let create_schedule_table =
  Sihl.Database.Migration.create_step
    ~label:"create registered schedule table"
    {sql|
      CREATE TABLE IF NOT EXISTS pool_schedules (
        `id` bigint(20) unsigned NOT NULL AUTO_INCREMENT,
        `label` varchar(255) NOT NULL,
        `scheduled_time` timestamp NULL,
        `scheduled_time_span` integer NULL,
        `status` varchar(128) NOT NULL,
        `last_run` timestamp NULL,
        `created_at` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP,
        `updated_at` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
      PRIMARY KEY (id),
      UNIQUE KEY `unique_label` (`label`)
      ) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci
    |sql}
;;

let migration () =
  Sihl.Database.Migration.(empty "schedule" |> add_step create_schedule_table)
;;
