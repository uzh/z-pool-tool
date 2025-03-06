let drop_falsy_unique_constraints_assignments =
  Database.Migration.Step.create
    ~label:"drop_falsy_unique_constraints_assignments"
    [%string
      {sql|
        ALTER TABLE pool_queue_job_assignment
        DROP INDEX unique_queue_uuid;
      |sql}]
;;

let drop_falsy_unique_constraints_experiment =
  Database.Migration.Step.create
    ~label:"drop_falsy_unique_constraints_experiment"
    [%string
      {sql|
        ALTER TABLE pool_queue_job_experiment
        DROP INDEX unique_queue_uuid;
      |sql}]
;;

let drop_falsy_unique_constraints_invitation =
  Database.Migration.Step.create
    ~label:"drop_falsy_unique_constraints_invitation"
    [%string
      {sql|
        ALTER TABLE pool_queue_job_invitation
        DROP INDEX unique_queue_uuid;
      |sql}]
;;

let drop_falsy_unique_constraints_session =
  Database.Migration.Step.create
    ~label:"drop_falsy_unique_constraints_session"
    [%string
      {sql|
        ALTER TABLE pool_queue_job_session
        DROP INDEX unique_queue_uuid;
      |sql}]
;;

let drop_falsy_unique_constraints_user =
  Database.Migration.Step.create
    ~label:"drop_falsy_unique_constraints_user"
    [%string
      {sql|
        ALTER TABLE pool_queue_job_user
        DROP INDEX unique_queue_uuid;
      |sql}]
;;

let migration () =
  Database.Migration.(
    empty "202503061915"
    |> add_step drop_falsy_unique_constraints_assignments
    |> add_step drop_falsy_unique_constraints_experiment
    |> add_step drop_falsy_unique_constraints_invitation
    |> add_step drop_falsy_unique_constraints_session
    |> add_step drop_falsy_unique_constraints_user)
;;
