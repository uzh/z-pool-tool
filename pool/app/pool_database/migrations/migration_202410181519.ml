let create_versions_table =
  Database.Migration.Step.create
    ~label:"create pool versions table"
    {sql|
      CREATE TABLE IF NOT EXISTS pool_versions (
        id BIGINT UNSIGNED AUTO_INCREMENT,
        uuid BINARY(16) NOT NULL,
        tag VARCHAR(128) NOT NULL,
        `text` TEXT NOT NULL,
        published_at timestamp NULL,
        created_at timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP,
        updated_at timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
        PRIMARY KEY (id),
        CONSTRAINT unique_tag UNIQUE KEY (tag)
      ) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci
  |sql}
;;

let add_guardian_role_permission =
  Database.Migration.Step.create
    ~label:"add default guardian role permissions"
    {sql|
    INSERT INTO `guardian_role_permissions` (`role`, `permission`, `target_model`) VALUES
    ('`Operator', 'manage', '`Version')
    ON DUPLICATE KEY UPDATE updated_at=updated_at
    |sql}
;;

let migration () =
  Database.Migration.(
    empty "202410181519"
    |> add_step create_versions_table
    |> add_step add_guardian_role_permission)
;;
