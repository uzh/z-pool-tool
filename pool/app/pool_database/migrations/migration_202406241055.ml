let create_text_message_dlr_table =
  Database.Migration.Step.create
    ~label:"add text gtx_sender to pool_tenants"
    {sql|
      CREATE TABLE IF NOT EXISTS pool_text_message_dlr (
        `id` bigint(20) unsigned NOT NULL AUTO_INCREMENT,
        `queue_job_uuid` binary(16) NOT NULL,
        `raw` text NOT NULL,
        `from` varchar(255),
        `to` varchar(255),
        `message_id` varchar(255),
        `dlr_mask` varchar(128),
        `error_code` integer, 
        `error_message` text,
        `submit_date` timestamp,
        `done_date` timestamp,
        `plmn` varchar(255),
        `country` varchar(255),
        `sms_cost` double,
        `created_at` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP,
        `updated_at` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
      PRIMARY KEY (id),
      UNIQUE KEY `unique_queue_job_uuid` (`queue_job_uuid`)
    ) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci
    |sql}
;;

let migration () =
  Database.Migration.(
    empty "202406241055" |> add_step create_text_message_dlr_table)
;;
