let create_pool_announcements_table =
  Database.Migration.Step.create
    ~label:"create pool_announcements table"
    {sql|
      CREATE TABLE IF NOT EXISTS pool_announcements (
        id BIGINT UNSIGNED AUTO_INCREMENT,
        uuid BINARY(16) NOT NULL,
        `text` text,
        start_at timestamp,
        end_at timestamp,
        created_at timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP,
        updated_at timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
        PRIMARY KEY (id),
        CONSTRAINT unique_uuid UNIQUE KEY (uuid)
      ) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci
    |sql}
;;

let create_pool_announcement_tenants_table =
  Database.Migration.Step.create
    ~label:"create pool_announcement_tenants table"
    {sql|
      CREATE TABLE IF NOT EXISTS pool_announcement_tenants (
        id BIGINT UNSIGNED AUTO_INCREMENT,
        pool_announcement_uuid BINARY(16) NOT NULL,
        pool_tenant_uuid BINARY(16) NOT NULL,
        created_at timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP,
        updated_at timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
        PRIMARY KEY (id),
        CONSTRAINT unique_announchment_tenant UNIQUE KEY (pool_announcement_uuid, pool_tenant_uuid),
        CONSTRAINT fk_pool_announcement_uuid FOREIGN KEY (pool_announcement_uuid) REFERENCES pool_announcements(uuid) ON DELETE CASCADE,
        CONSTRAINT fk_pool_tenant_uuid FOREIGN KEY (pool_tenant_uuid) REFERENCES pool_tenant(uuid) ON DELETE CASCADE
      ) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci
    |sql}
;;

let migration () =
  Database.Migration.(
    empty "202410031211"
    |> add_step create_pool_announcements_table
    |> add_step create_pool_announcement_tenants_table)
;;
