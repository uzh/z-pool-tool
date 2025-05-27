let add_not_null_constraints =
  Database.Migration.Step.create
    ~label:"add not null constraints to pool_assignments"
    {sql|
      ALTER TABLE pool_assignments
        MODIFY COLUMN session_uuid binary(16) NOT NULL,
        MODIFY COLUMN contact_uuid binary(16) NOT NULL
    |sql}
;;

let add_experiment_assignment_without_session_flag =
  Database.Migration.Step.create
    ~label:"add experiment assignment_without_session flag"
    {sql|
      ALTER TABLE pool_experiments
        ADD COLUMN assignment_without_session boolean NOT NULL DEFAULT 0 AFTER experiment_type,
        ADD COLUMN survey_url VARCHAR(255) AFTER assignment_without_session
    |sql}
;;

let make_session_participant_counts_nullable =
  Database.Migration.Step.create
    ~label:"make session participant counts nullable"
    {sql|
      ALTER TABLE pool_sessions
        MODIFY max_participants SMALLINT UNSIGNED,
        MODIFY min_participants SMALLINT UNSIGNED,
        MODIFY overbook SMALLINT UNSIGNED
    |sql}
;;

let migration () =
  Database.Migration.(
    empty "202404101112"
    |> add_step add_not_null_constraints
    |> add_step add_experiment_assignment_without_session_flag
    |> add_step make_session_participant_counts_nullable)
;;
