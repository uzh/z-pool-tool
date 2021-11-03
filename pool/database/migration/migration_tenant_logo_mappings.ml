let create_tenant_table =
  Sihl.Database.Migration.create_step
    ~label:"create tenant_logo_mappings table"
    {sql|
      CREATE TABLE IF NOT EXISTS pool_tenant_logo_mappings (
        `id` bigint(20) unsigned NOT NULL AUTO_INCREMENT,
        `uuid` binary(16) NOT NULL,
        `tenant_uuid` binary(16),
        `asset_uuid` binary(16),
        `logo_type` varchar(128) NOT NULL,
        `created_at` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP,
        `updated_at` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
      PRIMARY KEY (id),
      UNIQUE KEY `unique_uuid` (`uuid`)
      ) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;
    |sql}
;;

let migration () =
  Sihl.Database.Migration.(
    empty "tenant_logo_mappings" |> add_step create_tenant_table)
;;
