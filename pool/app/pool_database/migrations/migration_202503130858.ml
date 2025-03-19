let update_job_queue_mapping_fk_constraint
      ~table_name
      ~constraint_name
      ~updated_constraint
  =
  let drop_constraint =
    Database.Migration.Step.create
      ~label:(Format.asprintf "drop job queue mapping fk_constraint of %s" table_name)
      [%string
        {sql|
          ALTER TABLE %{table_name} DROP FOREIGN KEY %{constraint_name};
      |sql}]
  in
  let add_constraint =
    Database.Migration.Step.create
      ~label:(Format.asprintf "add updated fk_constraint to %s" table_name)
      [%string
        {sql|
          ALTER TABLE %{table_name} 
          ADD CONSTRAINT %{constraint_name}
          %{updated_constraint}
          ON DELETE CASCADE;
      |sql}]
  in
  [ drop_constraint; add_constraint ]
;;

let update_assignment_fk =
  let table_name = "pool_queue_job_assignment" in
  let constraint_name = "fk_pool_queue_job_assignment" in
  let updated_constraint =
    "FOREIGN KEY (assignment_uuid) REFERENCES pool_assignments(uuid)"
  in
  update_job_queue_mapping_fk_constraint ~table_name ~constraint_name ~updated_constraint
;;

let update_experiment_fk =
  let table_name = "pool_queue_job_experiment" in
  let constraint_name = "fk_pool_queue_job_experiment" in
  let updated_constraint =
    "FOREIGN KEY (experiment_uuid) REFERENCES pool_experiments(uuid)"
  in
  update_job_queue_mapping_fk_constraint ~table_name ~constraint_name ~updated_constraint
;;

let update_invitation_fk =
  let table_name = "pool_queue_job_invitation" in
  let constraint_name = "fk_pool_queue_job_invitation" in
  let updated_constraint =
    "FOREIGN KEY (invitation_uuid) REFERENCES pool_invitations(uuid)"
  in
  update_job_queue_mapping_fk_constraint ~table_name ~constraint_name ~updated_constraint
;;

let update_session_fk =
  let table_name = "pool_queue_job_session" in
  let constraint_name = "fk_pool_queue_job_session" in
  let updated_constraint = "FOREIGN KEY (session_uuid) REFERENCES pool_sessions(uuid)" in
  update_job_queue_mapping_fk_constraint ~table_name ~constraint_name ~updated_constraint
;;

let update_user_fk =
  let table_name = "pool_queue_job_user" in
  let constraint_name = "fk_pool_queue_job_user" in
  let updated_constraint = "FOREIGN KEY (user_uuid) REFERENCES user_users(uuid)" in
  update_job_queue_mapping_fk_constraint ~table_name ~constraint_name ~updated_constraint
;;

let migration () =
  let migration_steps =
    CCList.flatten
      [ update_assignment_fk
      ; update_experiment_fk
      ; update_invitation_fk
      ; update_session_fk
      ; update_user_fk
      ]
  in
  let open Database.Migration in
  CCList.fold_left
    (fun acc step -> add_step step acc)
    (empty "202503130858")
    migration_steps
;;
