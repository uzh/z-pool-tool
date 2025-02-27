let create_pool_queue_jobs_users_table =
  Database.Migration.Step.create
    ~label:"create pool_queue_jobs_users_table"
    [%string
      {sql|
      CREATE TABLE pool_queue_job_user (
        queue_uuid binary(16) NOT NULL,
        user_uuid binary(16) NOT NULL,
        created_at timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP,
        updated_at timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
        UNIQUE KEY `unique_queue_uuid` (`queue_uuid`),
        UNIQUE KEY `unique_queue_entity_combination` (queue_uuid, user_uuid),
        CONSTRAINT fk_pool_queue_job_user FOREIGN KEY (user_uuid) REFERENCES user_users(uuid)
    ) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;
    |sql}]
;;

let insert_contacts_queue_jobs =
  Database.Migration.Step.create
    ~label:"insert contacts_queue_jobs"
    {sql|
      INSERT INTO pool_queue_job_user (queue_uuid, user_uuid, created_at, updated_at)
      SELECT queue_uuid, contact_uuid, created_at, updated_at
      FROM pool_queue_job_contact
    |sql}
;;

let insert_admin_queue_jobs =
  Database.Migration.Step.create
    ~label:"insert admin_queue_jobs"
    {sql|
      INSERT INTO pool_queue_job_user (queue_uuid, user_uuid, created_at, updated_at)
      SELECT queue_uuid, admin_uuid, created_at, updated_at
      FROM pool_queue_job_admin
    |sql}
;;

let drop_contacts_queue_jobs =
  Database.Migration.Step.create
    ~label:"drop pool_queue_job_contact"
    {sql|
      DROP TABLE IF EXISTS pool_queue_job_contact
    |sql}
;;

let drop_admins_queue_jobs =
  Database.Migration.Step.create
    ~label:"drop pool_queue_job_admin"
    {sql|
      DROP TABLE IF EXISTS pool_queue_job_admin
    |sql}
;;

let migration () =
  Database.Migration.(
    empty "202502271318"
    |> add_step create_pool_queue_jobs_users_table
    |> add_step insert_contacts_queue_jobs
    |> add_step insert_admin_queue_jobs
    |> add_step drop_contacts_queue_jobs
    |> add_step drop_admins_queue_jobs)
;;
