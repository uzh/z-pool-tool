let add_unique_combination =
  Database.Migration.Step.create
    ~label:"add unique combination to pool_queue_jobs_mapping"
    {sql|
      ALTER TABLE pool_queue_jobs_mapping
        ADD CONSTRAINT unique_queue_entity_combination UNIQUE (queue_uuid, entity_uuid);
  |sql}
;;

(* Experiment mapping table *)
let create_pool_queue_job_experiment_table =
  Database.Migration.Step.create
    ~label:"create pool_queue_job_experiment table"
    {sql|
      CREATE TABLE pool_queue_job_experiment (
        queue_uuid binary(16),
        experiment_uuid binary(16),
        created_at DATETIME,
        updated_at DATETIME,
        UNIQUE KEY `unique_queue_uuid` (`queue_uuid`),
        CONSTRAINT fk_experiment FOREIGN KEY (experiment_uuid) REFERENCES pool_experiments(uuid)
    ) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;
  |sql}
;;

let populate_pool_queue_job_experiment_table =
  Database.Migration.Step.create
    ~label:"populate pool_queue_job_experiment table"
    {sql|
      INSERT INTO pool_queue_job_experiment (queue_uuid, experiment_uuid, created_at, updated_at)
      SELECT queue_uuid, entity_uuid, Q.created_at, Q.updated_at
      FROM pool_queue_jobs_mapping Q
      INNER JOIN pool_experiments ON pool_experiments.uuid = entity_uuid
      INNER JOIN (SELECT uuid FROM pool_queue_jobs UNION SELECT uuid FROM pool_queue_jobs_history) AS J ON J.uuid = Q.queue_uuid
  |sql}
;;

(* Contact mapping table *)
let create_pool_queue_job_contact_table =
  Database.Migration.Step.create
    ~label:"create pool_queue_job_contact table"
    {sql|
      CREATE TABLE pool_queue_job_contact (
        queue_uuid binary(16),
        contact_uuid binary(16),
        created_at DATETIME,
        updated_at DATETIME,
        UNIQUE KEY `unique_queue_uuid` (`queue_uuid`),
        CONSTRAINT fk_contact FOREIGN KEY (contact_uuid) REFERENCES pool_contacts(user_uuid)
    ) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;
  |sql}
;;

let populate_pool_queue_job_contact_table =
  Database.Migration.Step.create
    ~label:"populate pool_queue_job_contact table"
    {sql|
      INSERT INTO pool_queue_job_contact (queue_uuid, contact_uuid, created_at, updated_at)
      SELECT queue_uuid, entity_uuid, Q.created_at, Q.updated_at
      FROM pool_queue_jobs_mapping Q
      INNER JOIN pool_contacts ON pool_contacts.user_uuid = entity_uuid
      INNER JOIN (SELECT uuid FROM pool_queue_jobs UNION SELECT uuid FROM pool_queue_jobs_history) AS J ON J.uuid = Q.queue_uuid
  |sql}
;;

(* Session mapping table *)
let create_pool_queue_job_sessions_table =
  Database.Migration.Step.create
    ~label:"create pool_queue_job_session table"
    {sql|
      CREATE TABLE pool_queue_job_session (
        queue_uuid binary(16),
        session_uuid binary(16),
        created_at DATETIME,
        updated_at DATETIME,
        UNIQUE KEY `unique_queue_uuid` (`queue_uuid`),
        CONSTRAINT fk_session FOREIGN KEY (session_uuid) REFERENCES pool_sessions(uuid)
    ) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;
  |sql}
;;

(* IMO, IGNORE SHOULD NOT BE REQUIRED??? *)
let populate_pool_queue_job_session_table =
  Database.Migration.Step.create
    ~label:"populate pool_queue_job_session table"
    {sql|
      INSERT IGNORE INTO pool_queue_job_session (queue_uuid, session_uuid, created_at, updated_at)
      SELECT queue_uuid, entity_uuid, Q.created_at, Q.updated_at
      FROM pool_queue_jobs_mapping Q
      INNER JOIN pool_sessions ON pool_sessions.uuid = entity_uuid
      INNER JOIN (SELECT uuid FROM pool_queue_jobs UNION SELECT uuid FROM pool_queue_jobs_history) AS J ON J.uuid = Q.queue_uuid
  |sql}
;;

(* Invitation mapping table *)
let create_pool_queue_job_invitations_table =
  Database.Migration.Step.create
    ~label:"create pool_queue_job_invitation table"
    {sql|
      CREATE TABLE pool_queue_job_invitation (
        queue_uuid binary(16),
        invitation_uuid binary(16),
        created_at DATETIME,
        updated_at DATETIME,
        UNIQUE KEY `unique_queue_uuid` (`queue_uuid`),
        CONSTRAINT fk_invitation FOREIGN KEY (invitation_uuid) REFERENCES pool_invitations(uuid)
    ) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;
  |sql}
;;

let populate_invitations_mapping_table_name = "PopulateInvitationsMappingTable"

let make_populate_pool_queue_job_invitations_table_procedure =
  Migration_utils.chunked
    ~name:populate_invitations_mapping_table_name
    ~count_from:"pool_queue_job_contact"
    {sql|
      INSERT INTO pool_queue_job_invitation
      SELECT
          C.queue_uuid,
          I.uuid,
          C.created_at,
          C.updated_at
      FROM
          pool_queue_job_contact C
          INNER JOIN pool_queue_job_experiment E ON C.queue_uuid = E.queue_uuid
          INNER JOIN (SELECT uuid, message_template FROM pool_queue_jobs UNION SELECT uuid, message_template FROM pool_queue_jobs_history) as Q ON Q.uuid = C.queue_uuid
          INNER JOIN pool_invitations I ON I.experiment_uuid = E.experiment_uuid AND I.contact_uuid = C.contact_uuid
      WHERE
          Q.message_template = "experiment_invitation"
      ORDER BY
          C.queue_uuid
    |sql}
  |> Database.Migration.Step.create ~label:"populate pool_queue_job_invitations table"
;;

(* let create_intivation_sent_table =
  Database.Migration.Step.create
    ~label:"create pool_invitation_sent table"
    {sql|
      CREATE TABLE IF NOT EXISTS pool_invitation_reset (
        id BIGINT UNSIGNED AUTO_INCREMENT,
        invitation_uuid BINARY(16) NOT NULL,
        sent_at timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP,
        PRIMARY KEY (id),
        CONSTRAINT fk_invitation FOREIGN KEY (invitation_uuid) REFERENCES pool_invitations(uuid)
      ) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci
  |sql}
;; *)

(* TODO: How to migrate invitations with count > 2? *)
let insert_sent_timestamps =
  Migration_utils.chunked
    ~name:"InvitationsSentTimestamps"
    ~count_from:"pool_invitations"
    {sql|
      INSERT INTO pool_queue_jobs_history
        (uuid, name, input, message_template, tries, max_tries, run_at, status,
        persisted_at, handled_at, last_error, last_error_at, database_label, created_at, updated_at)
      SELECT
        uuid,
        name,
        input,
        hist.message_template,
        tries,
        max_tries,
        next_run_at,
        status,
        LEAST(last_error_at, next_run_at),
        last_error_at,
        last_error,
        IF(last_error IS NULL, NULL, last_error_at),
        SUBSTRING_INDEX(SUBSTRING_INDEX(`ctx`, '["pool","', - 1), '"]', 1),
        LEAST(last_error_at, next_run_at),
        GREATEST(last_error_at, next_run_at)
      FROM queue_jobs
      LEFT JOIN pool_message_history hist ON hist.queue_job_uuid = queue_jobs.uuid
      GROUP BY `uuid`
    |sql}
  |> Database.Migration.Step.create ~label:"insert invitations sent timestampts"
;;

let create_experimenet_invitation_reset_table =
  Database.Migration.Step.create
    ~label:"create pool_experiment_invitation_reset table"
    {sql|
      CREATE TABLE IF NOT EXISTS pool_experiment_invitation_reset (
        id BIGINT UNSIGNED AUTO_INCREMENT,
        experiment_uuid BINARY(16) NOT NULL,
        contacts_matching_filter INT UNSIGNED NOT NULL,
        sent_invitations INT UNSIGNED NOT NULL,
        created_at timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP,
        updated_at timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
        PRIMARY KEY (id),
        CONSTRAINT fk_experiment FOREIGN KEY (experiment_uuid) REFERENCES pool_experiments(uuid)
      ) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci
  |sql}
;;

let migration () =
  Database.Migration.(
    empty "202501091612"
    |> add_step create_pool_queue_job_experiment_table
    |> add_step populate_pool_queue_job_experiment_table
    |> add_step create_pool_queue_job_contact_table
    |> add_step populate_pool_queue_job_contact_table
    |> add_step create_pool_queue_job_sessions_table
    |> add_step populate_pool_queue_job_session_table
    |> add_step create_pool_queue_job_invitations_table
    |> add_step make_populate_pool_queue_job_invitations_table_procedure)
;;
