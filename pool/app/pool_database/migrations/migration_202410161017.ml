let create_sign_up_code_table =
  Database.Migration.Step.create
    ~label:"create sign_up codes table"
    {sql|
      CREATE TABLE IF NOT EXISTS pool_signup_codes (
        id BIGINT UNSIGNED AUTO_INCREMENT,
        uuid BINARY(16) NOT NULL,
        code VARCHAR(128) NOT NULL,
        count BIGINT UNSIGNED  NOT NULL DEFAULT 0,
        created_at timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP,
        updated_at timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
        PRIMARY KEY (id),
        CONSTRAINT unique_uuid UNIQUE KEY (uuid),
        CONSTRAINT unique_code UNIQUE KEY (code)
      ) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci
  |sql}
;;

let add_guardian_role_permission =
  Database.Migration.Step.create
    ~label:"add default guardian role permissions"
    {sql|
    INSERT INTO `guardian_role_permissions` (`role`, `permission`, `target_model`) VALUES
      ('`Operator', 'manage', '`SignupCode'),
      ('`Recruiter', 'manage', '`SignupCode')
    ON DUPLICATE KEY UPDATE updated_at=updated_at
    |sql}
;;

let migration () =
  Database.Migration.(
    empty "202410161017"
    |> add_step create_sign_up_code_table
    |> add_step add_guardian_role_permission)
;;
