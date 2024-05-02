let add_tenant_database_table =
  Database.Migration.Step.create
    ~label:"add tenant database table"
    {sql|
      CREATE TABLE IF NOT EXISTS pool_tenant_databases (
        `id` bigint(20) unsigned NOT NULL AUTO_INCREMENT,
        `label` varchar(255) NOT NULL,
        `url` varchar(255) NOT NULL,
        `status` varchar(128) NOT NULL DEFAULT 'active',
        `created_at` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP,
        `updated_at` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
      PRIMARY KEY (id),
      UNIQUE KEY `unique_label` (`label`)
      ) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci
    |sql}
;;

let insert_existing_databases =
  Database.Migration.Step.create
    ~label:"insert existing databases"
    {sql|
      INSERT INTO pool_tenant_databases (label, url)
      SELECT database_label, database_url FROM pool_tenant
    |sql}
;;

let add_foreign_key_on_label_and_drop_url =
  Database.Migration.Step.create
    ~label:"add foreign key for database label and drop database url"
    {sql|
      ALTER TABLE pool_tenant
        ADD CONSTRAINT fk_pool_tenant_database_label
        FOREIGN KEY (database_label) REFERENCES pool_tenant_databases(label),
        RENAME COLUMN mainenance TO maintenance,
        DROP COLUMN database_url
    |sql}
;;

let set_status_if_disabled =
  Database.Migration.Step.create
    ~label:"set status if disabled"
    {sql|
      UPDATE pool_tenant_databases
      JOIN pool_tenant ON pool_tenant.database_label = pool_tenant_databases.label
      SET pool_tenant_databases.status = 'disabled'
      WHERE pool_tenant.disabled = 1
    |sql}
;;

let set_status_if_maintenance =
  Database.Migration.Step.create
    ~label:"set status if maintenance"
    {sql|
      UPDATE pool_tenant_databases
      JOIN pool_tenant ON pool_tenant.database_label = pool_tenant_databases.label
      SET pool_tenant.status = 'maintenance'
      WHERE pool_tenant.maintenance = 1
    |sql}
;;

let remove_default_from_status_column =
  Database.Migration.Step.create
    ~label:"remove default from status column"
    {sql| ALTER TABLE pool_tenant_databases ALTER `status` DROP DEFAULT |sql}
;;

let remove_status_columns_from_pool_tenant =
  Database.Migration.Step.create
    ~label:"remove status columns from pool tenant"
    {sql|
      ALTER TABLE pool_tenant
        DROP COLUMN disabled,
        DROP COLUMN maintenance
    |sql}
;;

let migration () =
  Sihl.Database.Migration.(
    empty "202404051544"
    |> add_step add_tenant_database_table
    |> add_step insert_existing_databases
    |> add_step add_foreign_key_on_label_and_drop_url
    |> add_step set_status_if_disabled
    |> add_step remove_default_from_status_column
    |> add_step remove_status_columns_from_pool_tenant)
;;
