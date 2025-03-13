let create_mapping_table ~table_suffix ~column_name ~fk_constraint =
  Database.Migration.Step.create
    ~label:"add unique combination to pool_queue_jobs_mapping"
    [%string
      {sql|
      CREATE TABLE pool_queue_job_%{table_suffix} (
        queue_uuid binary(16) NOT NULL,
        %{column_name} binary(16) NOT NULL,
        created_at timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP,
        updated_at timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
        UNIQUE KEY `unique_queue_uuid` (`queue_uuid`),
        UNIQUE KEY `unique_queue_entity_combination` (queue_uuid, %{column_name}),
        %{fk_constraint}
    ) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;
    |sql}]
;;

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
  let table_suffix = "experiment" in
  let column_name = "experiment_uuid" in
  let fk_constraint =
    {|CONSTRAINT fk_pool_queue_job_experiment FOREIGN KEY (experiment_uuid) REFERENCES pool_experiments(uuid)|}
  in
  create_mapping_table ~table_suffix ~column_name ~fk_constraint
;;

let populate_pool_queue_job_experiment_table =
  Database.Migration.Step.create
    ~label:"populate pool_queue_job_experiment table"
    {sql|
      INSERT IGNORE INTO pool_queue_job_experiment (queue_uuid, experiment_uuid, created_at, updated_at)
      SELECT queue_uuid, entity_uuid, Q.created_at, Q.updated_at
      FROM pool_queue_jobs_mapping Q
      INNER JOIN pool_experiments ON pool_experiments.uuid = entity_uuid
      INNER JOIN (SELECT uuid FROM pool_queue_jobs UNION SELECT uuid FROM pool_queue_jobs_history) AS J ON J.uuid = Q.queue_uuid
  |sql}
;;

(* Contact mapping table *)
let create_pool_queue_job_contact_table =
  let table_suffix = "contact" in
  let column_name = "contact_uuid" in
  let fk_constraint =
    {|CONSTRAINT fk_pool_queue_job_contact FOREIGN KEY (contact_uuid) REFERENCES pool_contacts(user_uuid)|}
  in
  create_mapping_table ~table_suffix ~column_name ~fk_constraint
;;

let populate_pool_queue_job_contact_table =
  Database.Migration.Step.create
    ~label:"populate pool_queue_job_contact table"
    {sql|
      INSERT IGNORE INTO pool_queue_job_contact (queue_uuid, contact_uuid, created_at, updated_at)
      SELECT queue_uuid, entity_uuid, Q.created_at, Q.updated_at
      FROM pool_queue_jobs_mapping Q
      INNER JOIN pool_contacts ON pool_contacts.user_uuid = entity_uuid
      INNER JOIN (SELECT uuid FROM pool_queue_jobs UNION SELECT uuid FROM pool_queue_jobs_history) AS J ON J.uuid = Q.queue_uuid
  |sql}
;;

(* Session mapping table *)
let create_pool_queue_job_sessions_table =
  let table_suffix = "session" in
  let column_name = "session_uuid" in
  let fk_constraint =
    {|CONSTRAINT fk_pool_queue_job_session FOREIGN KEY (session_uuid) REFERENCES pool_sessions(uuid)|}
  in
  create_mapping_table ~table_suffix ~column_name ~fk_constraint
;;

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
  let table_suffix = "invitation" in
  let column_name = "invitation_uuid" in
  let fk_constraint =
    {|CONSTRAINT fk_pool_queue_job_invitation FOREIGN KEY (invitation_uuid) REFERENCES pool_invitations(uuid)|}
  in
  create_mapping_table ~table_suffix ~column_name ~fk_constraint
;;

let populate_pool_queue_job_invitations_table =
  Database.Migration.Step.create
    ~label:"populate pool_queue_job_invitations table"
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
      ORDER BY
          C.queue_uuid
  |sql}
;;

(* Assignment mapping table *)
let create_pool_queue_job_assignments_table =
  let table_suffix = "assignment" in
  let column_name = "assignment_uuid" in
  let fk_constraint =
    {|CONSTRAINT fk_pool_queue_job_assignment FOREIGN KEY (assignment_uuid) REFERENCES pool_assignments(uuid)|}
  in
  create_mapping_table ~table_suffix ~column_name ~fk_constraint
;;

let populate_pool_queue_job_assignments_table =
  Database.Migration.Step.create
    ~label:"populate pool_queue_job_assignment table"
    {sql|
      INSERT IGNORE INTO pool_queue_job_assignment (queue_uuid, assignment_uuid, created_at, updated_at)
      SELECT queue_uuid, entity_uuid, Q.created_at, Q.updated_at
      FROM pool_queue_jobs_mapping Q
      INNER JOIN pool_assignments ON pool_assignments.uuid = entity_uuid
      INNER JOIN (SELECT uuid FROM pool_queue_jobs UNION SELECT uuid FROM pool_queue_jobs_history) AS J ON J.uuid = Q.queue_uuid
  |sql}
;;

(* Admin mapping table *)
let create_pool_queue_job_admin_table =
  let table_suffix = "admin" in
  let column_name = "admin_uuid" in
  let fk_constraint =
    {|CONSTRAINT fk_admin FOREIGN KEY (admin_uuid) REFERENCES pool_admins(user_uuid)|}
  in
  create_mapping_table ~table_suffix ~column_name ~fk_constraint
;;

let populate_pool_queue_job_admin_table =
  Database.Migration.Step.create
    ~label:"populate pool_queue_job_admin table"
    {sql|
      INSERT IGNORE INTO pool_queue_job_admin (queue_uuid, admin_uuid, created_at, updated_at)
      SELECT queue_uuid, entity_uuid, Q.created_at, Q.updated_at
      FROM pool_queue_jobs_mapping Q
      INNER JOIN pool_admins ON pool_admins.user_uuid = entity_uuid
      INNER JOIN (SELECT uuid FROM pool_queue_jobs UNION SELECT uuid FROM pool_queue_jobs_history) AS J ON J.uuid = Q.queue_uuid
  |sql}
;;

let drop_mapping_table =
  Database.Migration.Step.create
    ~label:"drop pool_queue_jobs_mapping"
    {sql| DROP TABLE IF EXISTS pool_queue_jobs_mapping |sql}
;;

let create_experiment_invitation_reset_table =
  Database.Migration.Step.create
    ~label:"create pool_experiment_invitation_reset table"
    {sql|
      CREATE TABLE IF NOT EXISTS pool_experiment_invitation_reset (
        id BIGINT UNSIGNED AUTO_INCREMENT,
        experiment_uuid BINARY(16) NOT NULL,
        contacts_matching_filter INT NOT NULL,
        sent_invitations INT UNSIGNED NOT NULL,
        created_at timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP,
        updated_at timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
        PRIMARY KEY (id),
        CONSTRAINT fk_experiment_invitation_reset_xperiment FOREIGN KEY (experiment_uuid) REFERENCES pool_experiments(uuid)
      ) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci
  |sql}
;;

let populate_pool_experiment_invitation_reset =
  Database.Migration.Step.create
    ~label:"populate pool_experiment_invitation_reset"
    {sql|
      INSERT INTO
        pool_experiment_invitation_reset (experiment_uuid, contacts_matching_filter, sent_invitations, created_at, updated_at)
      WITH
        experiment_reset_at AS (
          SELECT
            title,
            experiment_uuid,
            invitation_reset_at,
            pool_experiments.created_at,
            MAX(send_count) AS max_count
          FROM
            pool_invitations
            INNER JOIN pool_experiments ON pool_experiments.uuid = pool_invitations.experiment_uuid
          GROUP BY
            experiment_uuid
          HAVING
            max_count = 2
            AND invitation_reset_at IS NOT NULL
        ),
        computed_resets AS (
          SELECT
            E.uuid,
            E.invitation_reset_at,
            COUNT(I.uuid) AS sent_invitations
          FROM
            experiment_reset_at
            INNER JOIN pool_experiments E ON E.uuid = experiment_reset_at.experiment_uuid
            INNER JOIN pool_invitations I ON I.experiment_uuid = E.uuid
            AND I.created_at <= E.invitation_reset_at
          GROUP BY
            E.uuid
        )
      SELECT
        uuid,
        -1,
        sent_invitations,
        invitation_reset_at,
        invitation_reset_at
      FROM
        computed_resets
  |sql}
;;

(* Due to the promoted contacts, FOREIGN KEY to the user table *)
let add_pool_invitations_user_users_fk =
  Database.Migration.Step.create
    ~label:"add unique combination to pool_queue_jobs_mapping"
    {sql|
      ALTER TABLE pool_invitations
        ADD CONSTRAINT FOREIGN KEY (contact_uuid) REFERENCES user_users(uuid);
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
    |> add_step create_pool_queue_job_assignments_table
    |> add_step populate_pool_queue_job_assignments_table
    |> add_step create_pool_queue_job_admin_table
    |> add_step populate_pool_queue_job_admin_table
    |> add_step create_pool_queue_job_invitations_table
    |> add_step populate_pool_queue_job_invitations_table
    |> add_step create_experiment_invitation_reset_table
    |> add_step populate_pool_experiment_invitation_reset
    |> add_step add_pool_invitations_user_users_fk)
;;
