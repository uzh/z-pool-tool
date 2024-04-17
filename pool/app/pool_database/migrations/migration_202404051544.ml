let add_tenant_database_table =
  Database.Migration.Step.create
    ~label:"add tenant database table"
    {sql|
      CREATE TABLE IF NOT EXISTS pool_tenant_databases (
        `id` bigint(20) unsigned NOT NULL AUTO_INCREMENT,
        `label` varchar(255) NOT NULL,
        `url` varchar(255) NOT NULL,
        `disabled` boolean NOT NULL DEFAULT FALSE,
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

let migration () =
  Sihl.Database.Migration.(
    empty "202404051544"
    |> add_step add_tenant_database_table
    |> add_step insert_existing_databases
    |> add_step add_foreign_key_on_label_and_drop_url)
;;
