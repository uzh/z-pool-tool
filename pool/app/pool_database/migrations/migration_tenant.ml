let create_tenant_table =
  Database.Migration.Step.create
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

let change_description_column_type =
  Database.Migration.Step.create
    ~label:"change description column type"
    {sql|
      ALTER TABLE pool_tenant
        MODIFY description text
    |sql}
;;

let create_smtp_table =
  Database.Migration.Step.create
    ~label:"create smtp table"
    {sql|
      CREATE TABLE IF NOT EXISTS pool_smtp (
        `id` bigint(20) unsigned NOT NULL AUTO_INCREMENT,
        `uuid` binary(16) NOT NULL,
        `label` varchar(255) NOT NULL,
        `server` varchar(255) NOT NULL,
        `port` integer NOT NULL,
        `username` varchar(255) NULL,
        `password` varchar(255) NULL,
        `mechanism` varchar(255) NOT NULL,
        `protocol` varchar(255) NOT NULL,
        `created_at` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP,
        `updated_at` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
      PRIMARY KEY (id),
      UNIQUE KEY `unique_uuid` (`uuid`),
      UNIQUE KEY `unique_label` (`label`)
      ) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci
    |sql}
;;

let remove_smtp_from_tenant_table =
  Database.Migration.Step.create
    ~label:"remove smtp columns from tenant table"
    {sql|
      ALTER TABLE pool_tenant
        DROP smtp_auth_server,
        DROP smtp_auth_port,
        DROP smtp_auth_username,
        DROP smtp_auth_password,
        DROP smtp_auth_authentication_method,
        DROP smtp_auth_protocol
    |sql}
;;

let make_styles_and_icon_nullable =
  Database.Migration.Step.create
    ~label:"make styles nullable"
    {sql|
      ALTER TABLE pool_tenant
        MODIFY styles binary(16),
        MODIFY icon binary(16)
    |sql}
;;

let migration_root () =
  Database.Migration.(
    empty "tenant"
    |> add_step create_tenant_table
    |> add_step change_description_column_type
    |> add_step remove_smtp_from_tenant_table
    |> add_step create_smtp_table
    |> add_step make_styles_and_icon_nullable)
;;

let migration_tenant () =
  Database.Migration.(empty "tenant" |> add_step create_smtp_table)
;;
