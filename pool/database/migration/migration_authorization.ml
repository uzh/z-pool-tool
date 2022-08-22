let create_authorization_tables =
  Sihl.Database.Migration.create_step
    ~label:"create tables for authorization system."
    {sql|
    CREATE TABLE IF NOT EXISTS entities (
      `id` TEXT UNIQUE NOT NULL,
      `roles` TEXT NOT NULL,
      `parent` TEXT,
      UNIQUE KEY `unique_id` (`id`)
    ) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci
    |sql}
;;

let create_rule_table =
  Sihl.Database.Migration.create_step
    ~label:"Create table for authorization rules"
    {sql|
    CREATE TABLE IF NOT EXISTS rules (
      `actor_id` TEXT,
      `actor_role` TEXT,
      `act` TEXT NOT NULL,
      `target_id` TEXT,
      `target_role` TEXT,
      -- These constraints are necessary to prevent rules that cannot be
      -- represented within OCaml.
      CONSTRAINT only_one_actor
          CHECK(
              (actor_id IS NULL OR actor_role IS NULL) 
              and (actor_id IS NOT NULL OR actor_role IS NOT NULL)
          ),
      CONSTRAINT only_one_target
          CHECK(
              (target_id IS NULL OR target_role IS NULL) 
              and (target_id IS NOT NULL OR target_role IS NOT NULL)
          ),
      UNIQUE(actor_role, act, target_role),
      UNIQUE(actor_role, act, target_id),
      UNIQUE(actor_id, act, target_role),
      UNIQUE(actor_id, act, target_id)
  ) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci
    |sql}
;;

let migration () =
  Sihl.Database.Migration.(
    empty "authorization"
    |> add_step create_authorization_tables
    |> add_step create_rule_table)
;;
