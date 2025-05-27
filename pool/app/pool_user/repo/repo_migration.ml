open Database.Migration

let fix_collation =
  Step.create
    ~label:"fix collation"
    {sql|
      SET collation_server = 'utf8mb4_unicode_ci'
    |sql}
;;

let create_users_table =
  Step.create
    ~label:"create users table"
    {sql|
      CREATE TABLE IF NOT EXISTS user_users (
        id BIGINT UNSIGNED AUTO_INCREMENT,
        uuid BINARY(16) NOT NULL,
        email VARCHAR(128) NOT NULL,
        password VARCHAR(128) NOT NULL,
        username VARCHAR(128),
        status VARCHAR(128) NOT NULL,
        admin BOOLEAN NOT NULL DEFAULT false,
        confirmed BOOLEAN NOT NULL DEFAULT false,
        created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
        PRIMARY KEY (id),
        CONSTRAINT unique_uuid UNIQUE KEY (uuid),
        CONSTRAINT unique_email UNIQUE KEY (email)
      ) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci
    |sql}
;;

let add_updated_at_column =
  Step.create
    ~label:"add updated_at column"
    {sql|
      ALTER TABLE user_users
        ADD COLUMN updated_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP
    |sql}
;;

let add_name_columns =
  Step.create
    ~label:"add name columns"
    {sql|
      ALTER TABLE user_users
        ADD COLUMN name VARCHAR(128) NULL,
        ADD COLUMN given_name VARCHAR(128) NULL
    |sql}
;;

let migration () =
  empty "user"
  |> add_step fix_collation
  |> add_step create_users_table
  |> add_step add_updated_at_column
  |> add_step add_name_columns
;;
