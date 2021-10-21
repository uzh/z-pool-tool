let create_participant_table =
  Sihl.Database.Migration.create_step
    ~label:"create participant table"
    {sql|
      CREATE TABLE IF NOT EXISTS pool_participants (
        `id` bigint(20) unsigned NOT NULL AUTO_INCREMENT,
        `user_uuid` binary(16) NOT NULL,
        `recruitment_channel` varchar(128) NOT NULL,
        `terms_accepted_at` timestamp NOT NULL,
        `paused` boolean NOT NULL,
        `disabled` boolean NOT NULL,
        `verified` timestamp NULL,
        `created_at` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP,
        `updated_at` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
      PRIMARY KEY (id),
      UNIQUE KEY `unique_uuid` (`user_uuid`)
      ) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;
    |sql}
;;

let migration () =
  Sihl.Database.Migration.(
    empty "participant" |> add_step create_participant_table)
;;
