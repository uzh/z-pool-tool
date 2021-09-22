let create_tenant_table =
  Sihl.Database.Migration.create_step
    ~label:"create tenant table"
    {sql|
      CREATE TABLE IF NOT EXISTS pool_tenant (
        `id` bigint(20) unsigned NOT NULL AUTO_INCREMENT,
        `uuid` binary(16) NOT NULL,
        `title` varchar(255) COLLATE utf8mb4_unicode_ci NOT NULL,
        `description` varchar(255) COLLATE utf8mb4_unicode_ci,
        `url` varchar(255) COLLATE utf8mb4_unicode_ci NOT NULL,
        `database_url` varchar(255) COLLATE utf8mb4_unicode_ci NOT NULL,
        `smtp_server` varchar(255) COLLATE utf8mb4_unicode_ci NOT NULL,
        `smtp_port` varchar(128) COLLATE utf8mb4_unicode_ci NOT NULL,
        `smtp_username` varchar(255) COLLATE utf8mb4_unicode_ci NOT NULL,
        `smtp_authentication_method` varchar(255) COLLATE utf8mb4_unicode_ci NOT NULL,
        `smtp_protocol` varchar(255) COLLATE utf8mb4_unicode_ci NOT NULL,
        `styles` varchar(255) COLLATE utf8mb4_unicode_ci NOT NULL,
        `icon` varchar(255) COLLATE utf8mb4_unicode_ci NOT NULL,
        `logos` varchar(255) COLLATE utf8mb4_unicode_ci NOT NULL,
        `partner_logos` varchar(255) COLLATE utf8mb4_unicode_ci NOT NULL,
        `mainenance` boolean NOT NULL,
        `disabled` boolean NOT NULL,
        `default_language` varchar(128) COLLATE utf8mb4_unicode_ci NOT NULL,
        `created_at` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP,
        `updated_at` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
      PRIMARY KEY (id),
      UNIQUE KEY `unique_uuid` (`uuid`)
      ) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;
    |sql}
;;

let migration () =
  Sihl.Database.Migration.(empty "tenant" |> add_step create_tenant_table)
;;
