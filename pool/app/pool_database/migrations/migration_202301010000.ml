let create_jobs_table =
  Database.Migration.Step.create
    ~label:"create jobs table"
    {sql|
      CREATE TABLE IF NOT EXISTS queue_jobs (
        id BIGINT UNSIGNED AUTO_INCREMENT,
        uuid BINARY(16) NOT NULL,
        name VARCHAR(128) NOT NULL,
        input TEXT NOT NULL DEFAULT '',
        tries BIGINT UNSIGNED,
        next_run_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
        max_tries BIGINT UNSIGNED,
        status VARCHAR(128) NOT NULL,
        last_error TEXT,
        last_error_at TIMESTAMP,
        tag TEXT NULL,
        ctx TEXT NULL,
        PRIMARY KEY (id),
        CONSTRAINT unique_uuid UNIQUE KEY (uuid)
      ) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci
    |sql}
;;

let migration () = Database.Migration.(empty "202301010000" |> add_step create_jobs_table)
