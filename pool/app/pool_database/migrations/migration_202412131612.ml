let create_page_scripts_table =
  Database.Migration.Step.create
    ~label:"create pool versions table"
    {sql|
      CREATE TABLE IF NOT EXISTS pool_tenant_page_scripts (
        id BIGINT UNSIGNED AUTO_INCREMENT,
        uuid BINARY(16) NOT NULL,
        location VARCHAR(128) NOT NULL,
        script TEXT NULL,
        created_at timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP,
        updated_at timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
        PRIMARY KEY (id),
        CONSTRAINT unique_tag UNIQUE KEY (location)
      ) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci
  |sql}
;;

let migration () =
  Database.Migration.(empty "202412131612" |> add_step create_page_scripts_table)
;;
