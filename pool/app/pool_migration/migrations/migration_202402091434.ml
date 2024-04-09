let create_pool_message_history_table =
  Database.Migration.create_step
    ~label:"create pool_message_history table"
    {sql|
      CREATE TABLE IF NOT EXISTS pool_message_history (
        `id` bigint(20) unsigned NOT NULL AUTO_INCREMENT,
        `queue_job_uuid` binary(16) NOT NULL,
        `entity_uuid` binary(16) NOT NULL,
        `message_template` varchar(255),
        `created_at` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP,
        `updated_at` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
      PRIMARY KEY (id)
      ) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci
    |sql}
;;

let add_fk_contraint_to_entity_queue_jobs =
  Database.Migration.create_step
    ~label:"add fk contraint to entity_queue_jobs"
    {sql|
      ALTER TABLE pool_message_history
        ADD CONSTRAINT fk_pool_message_history
        FOREIGN KEY (queue_job_uuid) REFERENCES queue_jobs(uuid)
    |sql}
;;

let add_unique_contraint_to_entity_queue_jobs =
  Database.Migration.create_step
    ~label:"add unique contraint to entity_queue_jobs"
    {sql|
      ALTER TABLE pool_message_history
        ADD CONSTRAINT unique_queue_job_entity_combination UNIQUE (queue_job_uuid, entity_uuid)
    |sql}
;;

let add_entity_uuid_index =
  Database.Migration.create_step
    ~label:"add entity uuid index"
    {sql|
      CREATE INDEX entity_uuid ON pool_message_history (entity_uuid)
    |sql}
;;

let migration () =
  Sihl.Database.Migration.(
    empty "202402091434"
    |> add_step create_pool_message_history_table
    |> add_step add_fk_contraint_to_entity_queue_jobs
    |> add_step add_unique_contraint_to_entity_queue_jobs
    |> add_step add_entity_uuid_index)
;;
