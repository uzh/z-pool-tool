open Database

let create_pool_queue_job_authentication_table =
  Migration.Step.create
    ~label:"create pool_queue_job_authentication table"
    {sql|
      CREATE TABLE IF NOT EXISTS pool_queue_job_authentication (
        queue_uuid binary(16) NOT NULL,
        authentication_uuid binary(16) NOT NULL,
        created_at timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP,
        updated_at timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
        UNIQUE KEY `unique_queue_uuid` (`queue_uuid`),
        UNIQUE KEY `unique_queue_entity_combination` (queue_uuid, authentication_uuid),
        KEY `idx_authentication_uuid` (`authentication_uuid`),
        CONSTRAINT fk_pool_queue_job_authentication FOREIGN KEY (authentication_uuid) REFERENCES pool_authentication(uuid) ON DELETE CASCADE
      ) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;
    |sql}
;;

let remove_automagic_timestamp_on_update_trigger =
  Migration.Step.create
    ~label:
      "remove mariadbs automagic timestamp on update trigger for token valid_until column"
    {sql|
      ALTER TABLE pool_authentication
      MODIFY COLUMN valid_until TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP()
    |sql}
;;

let add_usage_count_to_authentication =
  Migration.Step.create
    ~label:"add usage_count to pool_authentication"
    {sql|
      ALTER TABLE pool_authentication
      ADD COLUMN usage_count TINYINT UNSIGNED NOT NULL DEFAULT 0 AFTER token
    |sql}
;;

let migration () =
  Migration.(
    empty "202607141227"
    |> add_step create_pool_queue_job_authentication_table
    |> add_step remove_automagic_timestamp_on_update_trigger
    |> add_step add_usage_count_to_authentication)
;;
