let create_api_keys_table =
  Database.Migration.Step.create
    ~label:"create pool_api_keys table"
    {sql|
      CREATE TABLE IF NOT EXISTS pool_api_keys (
        id BIGINT UNSIGNED AUTO_INCREMENT,
        uuid BINARY(16) NOT NULL,
        name VARCHAR(255) NOT NULL,
        token VARCHAR(255) NOT NULL,
        expires_at timestamp NOT NULL,
        created_at timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP,
        updated_at timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
        PRIMARY KEY (id),
        CONSTRAINT unique_uuid UNIQUE KEY (uuid),
        CONSTRAINT unique_token UNIQUE KEY (token)
      ) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci
    |sql}
;;

let create_api_keys_permissions =
  Database.Migration.Step.create
    ~label:"add pool_api_keys permission"
    {sql|
    INSERT INTO `guardian_role_permissions` (`role`, `permission`, `target_model`) VALUES
    ('`Operator', 'manage', '`ApiKey')
    ON DUPLICATE KEY UPDATE updated_at=updated_at;
  |sql}
;;

let migration () =
  Database.Migration.(
    empty "202410071409"
    |> add_step create_api_keys_table
    |> add_step create_api_keys_permissions)
;;
