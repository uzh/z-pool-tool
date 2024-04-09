let create_waiting_list_table =
  Database.Migration.create_step
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

let rename_subject_to_contact =
  Database.Migration.create_step
    ~label:"rename subject id to contact id"
    {sql|
      ALTER TABLE pool_waiting_list
        RENAME COLUMN subject_id TO contact_id
    |sql}
;;

let add_comment_to_waiting_list =
  Database.Migration.create_step
    ~label:"add comment to waiting list"
    {sql|
      ALTER TABLE pool_waiting_list
        ADD COLUMN comment text NULL AFTER experiment_id
    |sql}
;;

let migration () =
  Sihl.Database.Migration.(
    empty "waiting_list"
    |> add_step create_waiting_list_table
    |> add_step rename_subject_to_contact
    |> add_step add_comment_to_waiting_list)
;;
