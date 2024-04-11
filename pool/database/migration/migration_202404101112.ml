let add_not_null_constraints =
  Sihl.Database.Migration.create_step
    ~label:"add not null constraints to pool_assignments"
    {sql|
      ALTER TABLE pool_assignments 
        MODIFY COLUMN session_uuid binary(16) NOT NULL,
        MODIFY COLUMN contact_uuid binary(16) NOT NULL
    |sql}
;;

let add_experiment_assignment_without_session_flag =
  Sihl.Database.Migration.create_step
    ~label:"add experiment assignment_without_session flag"
    {sql|
      ALTER TABLE pool_experiments 
        ADD COLUMN assignment_without_session boolean NOT NULL DEFAULT 0 AFTER experiment_type,
        ADD COLUMN redirect_immediately boolean NOT NULL DEFAULT 0 AFTER assignment_without_session,
        ADD COLUMN survey_url VARCHAR(255) AFTER redirect_immediately
    |sql}
;;

let migration () =
  Sihl.Database.Migration.(
    empty "202404101112"
    |> add_step add_not_null_constraints
    |> add_step add_experiment_assignment_without_session_flag)
;;
