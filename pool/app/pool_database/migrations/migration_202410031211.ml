let create_pool_announcements_table =
  Database.Migration.Step.create
    ~label:"create pool_announcements table"
    {sql|
      CREATE TABLE IF NOT EXISTS pool_announcements (
        id BIGINT UNSIGNED AUTO_INCREMENT,
        uuid BINARY(16) NOT NULL,
        `text` text,
        start_at timestamp NULL,
        end_at timestamp NULL,
        show_to_admins BOOLEAN NOT NULL DEFAULT FALSE,
        show_to_contacts BOOLEAN NOT NULL DEFAULT FALSE,
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
        CONSTRAINT unique_announcement_tenant UNIQUE KEY (pool_announcement_uuid, pool_tenant_uuid),
        CONSTRAINT fk_pool_announcement_tenant_announcement_uuid FOREIGN KEY (pool_announcement_uuid) REFERENCES pool_announcements(uuid) ON DELETE CASCADE,
        CONSTRAINT fk_pool_tenant_uuid FOREIGN KEY (pool_tenant_uuid) REFERENCES pool_tenant(uuid) ON DELETE CASCADE
      ) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci
    |sql}
;;

let create_pool_announcement_users_hide_table =
  Database.Migration.Step.create
    ~label:"create pool_announcement_users_hide table"
    {sql|
      CREATE TABLE IF NOT EXISTS pool_announcement_users_hide (
        id BIGINT UNSIGNED AUTO_INCREMENT,
        pool_announcement_uuid BINARY(16) NOT NULL,
        user_users_uuid BINARY(16) NOT NULL,
        created_at timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP,
        updated_at timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
        PRIMARY KEY (id),
        CONSTRAINT fk_pool_announcement_user_announcement_uuid FOREIGN KEY (pool_announcement_uuid) REFERENCES pool_announcements(uuid) ON DELETE CASCADE,
        CONSTRAINT unique_announcement_contact UNIQUE KEY (pool_announcement_uuid, user_users_uuid)
      ) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci
    |sql}
;;

let add_guardian_role_permission =
  Database.Migration.Step.create
    ~label:"add default guardian role permissions"
    {sql|
    INSERT INTO `guardian_role_permissions` (`role`, `permission`, `target_model`) VALUES
    ('`Operator', 'manage', '`Announcement')
    ON DUPLICATE KEY UPDATE updated_at=updated_at
    |sql}
;;

let migration () =
  Database.Migration.(
    empty "202410031211"
    |> add_step create_pool_announcements_table
    |> add_step create_pool_announcement_tenants_table
    |> add_step create_pool_announcement_users_hide_table
    |> add_step add_guardian_role_permission)
;;
