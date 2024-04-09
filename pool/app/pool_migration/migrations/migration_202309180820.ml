let add_assignment_session_fk_constraint =
  Database.Migration.create_step
    ~label:"add assignment session fk constraint"
    {sql|
      ALTER TABLE pool_assignments
      ADD CONSTRAINT fk_pool_assignments_sessions
      FOREIGN KEY (session_uuid)
      REFERENCES pool_sessions (uuid)
      ON DELETE CASCADE;
    |sql}
;;

let add_assignment_contact_index =
  Database.Migration.create_step
    ~label:"add assignment contact index"
    {sql|
      CREATE INDEX index_contact_uuid ON pool_assignments (contact_uuid)
    |sql}
;;

let add_custom_field_answers_custom_field_fk_constraint =
  Database.Migration.create_step
    ~label:"add custom field answers custom field fk constraint"
    {sql|
      ALTER TABLE pool_custom_field_answers
      ADD CONSTRAINT fk_pool_custom_field_answers_pool_custom_fields
      FOREIGN KEY (custom_field_uuid)
      REFERENCES pool_custom_fields (uuid)
      ON DELETE RESTRICT;
    |sql}
;;

let add_custom_field_options_custom_field_fk_constraint =
  Database.Migration.create_step
    ~label:"add custom field options custom field fk constraint"
    {sql|
        ALTER TABLE pool_custom_field_options
        ADD CONSTRAINT fk_pool_custom_field_options_pool_custom_fields
        FOREIGN KEY (custom_field_uuid)
        REFERENCES pool_custom_fields (uuid)
        ON DELETE CASCADE;
      |sql}
;;

let add_custom_fields_custom_field_groups_fk_constraint =
  Database.Migration.create_step
    ~label:"add custom field custom field groups fk constraint"
    {sql|
        ALTER TABLE pool_custom_fields
        ADD CONSTRAINT fk_pool_custom_fields_custom_field_groups
        FOREIGN KEY (custom_field_group_uuid)
        REFERENCES pool_custom_field_groups (uuid)
        ON DELETE SET NULL;
      |sql}
;;

let add_email_verification_index =
  Database.Migration.create_step
    ~label:"add email verification index"
    {sql|
      CREATE INDEX index_user_uuid ON pool_email_verifications (user_uuid)
    |sql}
;;

let add_location_file_asseet_fk_constraint =
  Database.Migration.create_step
    ~label:"add location file asseet fk constraint"
    {sql|
        ALTER TABLE pool_location_file_mappings
        ADD CONSTRAINT fk_pool_file_mappings_storage_handles
        FOREIGN KEY (asset_id)
        REFERENCES storage_handles (id)
        ON DELETE CASCADE;
      |sql}
;;

let add_location_file_location_fk_constraint =
  Database.Migration.create_step
    ~label:"add location file location fk constraint"
    {sql|
        ALTER TABLE pool_location_file_mappings
        ADD CONSTRAINT fk_pool_file_mappings_locations
        FOREIGN KEY (location_id)
        REFERENCES pool_locations (id)
        ON DELETE CASCADE;
      |sql}
;;

let add_mailing_experiment_fk_constraint =
  Database.Migration.create_step
    ~label:"add mailing experiment fk constraint"
    {sql|
        ALTER TABLE pool_mailing
        ADD CONSTRAINT fk_pool_mailing_experiments
        FOREIGN KEY (experiment_uuid)
        REFERENCES pool_experiments (uuid)
        ON DELETE CASCADE;
      |sql}
;;

let add_session_experiment_fk_constraint =
  Database.Migration.create_step
    ~label:"add session experiment fk constraint"
    {sql|
        ALTER TABLE pool_sessions
        ADD CONSTRAINT fk_pool_session_experiments
        FOREIGN KEY (experiment_uuid)
        REFERENCES pool_experiments (uuid)
        ON DELETE RESTRICT;
      |sql}
;;

let add_experiment_filter_fk_constraint =
  Database.Migration.create_step
    ~label:"add session experiment fk constraint"
    {sql|
        ALTER TABLE pool_experiments
        ADD CONSTRAINT fk_pool_experiments_filter
        FOREIGN KEY (filter_uuid)
        REFERENCES pool_filter (uuid)
        ON DELETE SET NULL;
      |sql}
;;

let add_admin_sihl_user_fk_constraint =
  Database.Migration.create_step
    ~label:"add admin sihl user fk constraint"
    {sql|
        ALTER TABLE pool_admins
        ADD CONSTRAINT fk_pool_admins_sihl_user
        FOREIGN KEY (user_uuid)
        REFERENCES user_users (uuid)
        ON DELETE RESTRICT;
      |sql}
;;

let add_contact_sihl_user_fk_constraint =
  Database.Migration.create_step
    ~label:"add contact sihl user fk constraint"
    {sql|
        ALTER TABLE pool_contacts
        ADD CONSTRAINT fk_pool_contacts_sihl_user
        FOREIGN KEY (user_uuid)
        REFERENCES user_users (uuid)
        ON DELETE RESTRICT;
      |sql}
;;

let add_promoted_contact_sihl_user_fk_constraint =
  Database.Migration.create_step
    ~label:"add promoted contact sihl user fk constraint"
    {sql|
        ALTER TABLE pool_contacts_promoted
        ADD CONSTRAINT fk_pool_contacts_promoted_sihl_user
        FOREIGN KEY (user_uuid)
        REFERENCES user_users (uuid)
        ON DELETE RESTRICT;
      |sql}
;;

let add_waiting_list_experiment_fk_constraint =
  Database.Migration.create_step
    ~label:"add waiting list experiment fk constraint"
    {sql|
        ALTER TABLE pool_waiting_list
        ADD CONSTRAINT fk_pool_waiting_list_experiment
        FOREIGN KEY (experiment_uuid)
        REFERENCES pool_experiments (uuid)
        ON DELETE RESTRICT;
      |sql}
;;

let add_invitations_experiment_fk_constraint =
  Database.Migration.create_step
    ~label:"add invitations experiment fk constraint"
    {sql|
        ALTER TABLE pool_invitations
        ADD CONSTRAINT fk_pool_invitations_experiment
        FOREIGN KEY (experiment_uuid)
        REFERENCES pool_experiments (uuid)
        ON DELETE RESTRICT;
      |sql}
;;

let add_taggings_tags_fk_constraint =
  Database.Migration.create_step
    ~label:"add taggings tag fk constraint"
    {sql|
        ALTER TABLE pool_tagging
        ADD CONSTRAINT fk_pool_tagging_tag
        FOREIGN KEY (tag_uuid)
        REFERENCES pool_tags (uuid)
        ON DELETE RESTRICT;
      |sql}
;;

let add_participation_tags_tags_fk_constraint =
  Database.Migration.create_step
    ~label:"add participation tags tag fk constraint"
    {sql|
        ALTER TABLE pool_participation_tags
        ADD CONSTRAINT fk_pool_participation_tags_tag
        FOREIGN KEY (tag_uuid)
        REFERENCES pool_tags (uuid)
        ON DELETE RESTRICT;
      |sql}
;;

let migration () =
  Sihl.Database.Migration.(
    empty "202309180820"
    |> add_step add_assignment_session_fk_constraint
    |> add_step add_assignment_contact_index
    |> add_step add_custom_field_answers_custom_field_fk_constraint
    |> add_step add_custom_field_options_custom_field_fk_constraint
    |> add_step add_custom_fields_custom_field_groups_fk_constraint
    |> add_step add_email_verification_index
    |> add_step add_location_file_asseet_fk_constraint
    |> add_step add_location_file_location_fk_constraint
    |> add_step add_mailing_experiment_fk_constraint
    |> add_step add_session_experiment_fk_constraint
    |> add_step add_experiment_filter_fk_constraint
    |> add_step add_admin_sihl_user_fk_constraint
    |> add_step add_contact_sihl_user_fk_constraint
    |> add_step add_promoted_contact_sihl_user_fk_constraint
    |> add_step add_waiting_list_experiment_fk_constraint
    |> add_step add_invitations_experiment_fk_constraint
    |> add_step add_taggings_tags_fk_constraint
    |> add_step add_participation_tags_tags_fk_constraint)
;;

let add_system_event_logs_system_event_fk_constraint =
  Database.Migration.create_step
    ~label:"add system event logs system event fk constraint"
    {sql|
        ALTER TABLE pool_system_event_logs
        ADD CONSTRAINT fk_pool_system_event_logs_system_events
        FOREIGN KEY (event_uuid)
        REFERENCES pool_system_events (uuid)
        ON DELETE CASCADE;
      |sql}
;;

let add_tenant_logo_tenant_fk_constraint =
  Database.Migration.create_step
    ~label:"add tenant logo tenant fk constraint"
    {sql|
        ALTER TABLE pool_tenant_logo_mappings
        ADD CONSTRAINT fk_pool_tenant_logo_mappings_tenant
        FOREIGN KEY (tenant_uuid)
        REFERENCES pool_tenant (uuid)
        ON DELETE CASCADE;
      |sql}
;;

let add_tenant_logo_storage_handle_fk_constraint =
  Database.Migration.create_step
    ~label:"add tenant logo storage handle fk constraint"
    {sql|
        ALTER TABLE pool_tenant_logo_mappings
        ADD CONSTRAINT fk_pool_tenant_logo_mappings_storage_handles
        FOREIGN KEY (asset_uuid)
        REFERENCES storage_handles (uuid)
        ON DELETE CASCADE;
      |sql}
;;

let migration_root () =
  Sihl.Database.Migration.(
    empty "202309180820"
    |> add_step add_system_event_logs_system_event_fk_constraint
    |> add_step add_tenant_logo_tenant_fk_constraint
    |> add_step add_tenant_logo_storage_handle_fk_constraint)
;;
