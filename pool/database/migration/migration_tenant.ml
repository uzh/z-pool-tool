let create_tenant_table =
  Sihl.Database.Migration.create_step
    ~label:"create tenant table"
    {sql|
      CREATE TABLE IF NOT EXISTS pool_tenant (
        `id` bigint(20) unsigned NOT NULL AUTO_INCREMENT,
        `uuid` binary(16) NOT NULL,
        `title` varchar(255) NOT NULL,
        `description` varchar(255),
        `url` varchar(255) NOT NULL,
        `database_url` varchar(255) NOT NULL,
        `database_label` varchar(255) NOT NULL,
        `smtp_auth_server` varchar(255) NOT NULL,
        `smtp_auth_port` varchar(128) NOT NULL,
        `smtp_auth_username` varchar(255) NOT NULL,
        `smtp_auth_password` varchar(255) NOT NULL,
        `smtp_auth_authentication_method` varchar(255) NOT NULL,
        `smtp_auth_protocol` varchar(255) NOT NULL,
        `styles` binary(16) NOT NULL,
        `icon` binary(16) NOT NULL,
        `mainenance` boolean NOT NULL,
        `disabled` boolean NOT NULL,
        `default_language` varchar(128) NOT NULL,
        `created_at` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP,
        `updated_at` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
      PRIMARY KEY (id),
      UNIQUE KEY `unique_uuid` (`uuid`)
      ) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci
    |sql}
;;

let migration () =
  Sihl.Database.Migration.(empty "tenant" |> add_step create_tenant_table)
;;
