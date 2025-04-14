let create_gtx_api_keys_table =
  Database.Migration.Step.create
    ~label:"create pool_gtx_api_keys table"
    [%string
      {sql|
        CREATE TABLE IF NOT EXISTS pool_gtx_api_keys (
        id BIGINT UNSIGNED AUTO_INCREMENT,
        uuid BINARY(16) NOT NULL,
        api_key VARCHAR(255) NOT NULL,
        sender_name VARCHAR(128) NOT NULL,
        created_at timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP,
        updated_at timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
        PRIMARY KEY (id),
        CONSTRAINT unique_uuid UNIQUE KEY (uuid)
      ) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci
      |sql}]
;;

let migration () =
  Database.Migration.(empty "202504111642" |> add_step create_gtx_api_keys_table)
;;
