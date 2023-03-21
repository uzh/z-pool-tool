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

let rename_subject_to_contact =
  Sihl.Database.Migration.create_step
    ~label:"rename subject id to contact id"
    {sql|
      ALTER TABLE pool_waiting_list
        RENAME COLUMN subject_id TO contact_id
    |sql}
;;

let add_comment_to_waiting_list =
  Sihl.Database.Migration.create_step
    ~label:"add comment to waiting list"
    {sql|
      ALTER TABLE pool_waiting_list
        ADD COLUMN comment text NULL AFTER experiment_id
    |sql}
;;

let use_uuids_as_foreign_keys_in_waitinglist_table =
  Sihl.Database.Migration.create_step
    ~label:"replace ids with uuids as foreignkeys"
    {sql|
      BEGIN NOT ATOMIC
        ALTER TABLE pool_waiting_list
          ADD COLUMN contact_uuid binary(16) AFTER contact_id,
          ADD COLUMN experiment_uuid binary(16) AFTER experiment_id;

        UPDATE pool_waiting_list SET
          contact_uuid = (SELECT user_uuid FROM pool_contacts WHERE pool_contacts.id = pool_waiting_list.contact_id),
          experiment_uuid = (SELECT uuid FROM pool_experiments WHERE pool_experiments.id = pool_waiting_list.experiment_id);

        ALTER TABLE pool_waiting_list
          ADD CONSTRAINT unique_contact_experiment_combination UNIQUE (contact_uuid, experiment_uuid);

        ALTER TABLE pool_waiting_list
          MODIFY COLUMN `contact_uuid` binary(16) NOT NULL,
          MODIFY COLUMN `experiment_uuid` binary(16) NOT NULL;

        ALTER TABLE pool_waiting_list
          DROP COLUMN contact_id,
          DROP COLUMN experiment_id;
      END;
    |sql}
;;

let migration () =
  Sihl.Database.Migration.(
    empty "waiting_list"
    |> add_step create_waiting_list_table
    |> add_step rename_subject_to_contact
    |> add_step add_comment_to_waiting_list
    |> add_step use_uuids_as_foreign_keys_in_waitinglist_table)
;;
