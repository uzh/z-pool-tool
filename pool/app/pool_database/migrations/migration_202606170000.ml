let create_pool_queue_job_authentication_table =
  Database.Migration.Step.create
    ~label:"create pool_queue_job_authentication table"
    {sql|
      CREATE TABLE IF NOT EXISTS pool_queue_job_authentication (
        queue_uuid binary(16) NOT NULL,
        authentication_uuid binary(16) NOT NULL,
        created_at timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP,
        updated_at timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
        UNIQUE KEY `unique_queue_uuid` (`queue_uuid`),
        UNIQUE KEY `unique_queue_entity_combination` (queue_uuid, authentication_uuid),
        CONSTRAINT fk_pool_queue_job_authentication FOREIGN KEY (authentication_uuid) REFERENCES pool_authentication(uuid)
      ) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;
    |sql}
;;

(* This migration is executed on root and tenant databases *)
let migration () =
  Database.Migration.(
    empty "202606170000" |> add_step create_pool_queue_job_authentication_table)
;;
