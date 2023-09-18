let add_assignment_session_index =
  Sihl.Database.Migration.create_step
    ~label:"add assignment session index"
    {sql|
      CREATE INDEX index_session_uuid ON pool_assignments (session_uuid)
    |sql}
;;

let add_assignment_contact_index =
  Sihl.Database.Migration.create_step
    ~label:"add assignment contact index"
    {sql|
      CREATE INDEX index_contact_uuid ON pool_assignments (contact_uuid)
    |sql}
;;

let add_custom_field_answers_custom_field_uuid_index =
  Sihl.Database.Migration.create_step
    ~label:"add custom field answers custom field index"
    {sql|
      CREATE INDEX index_custom_field_uuid ON pool_custom_field_answers (custom_field_uuid)
    |sql}
;;

let add_custom_field_answers_entity_uuid_index =
  Sihl.Database.Migration.create_step
    ~label:"add custom field answers entity index"
    {sql|
      CREATE INDEX index_entity_uuid ON pool_custom_field_answers (entity_uuid)
    |sql}
;;

let add_custom_field_options_index =
  Sihl.Database.Migration.create_step
    ~label:"add custom field options index"
    {sql|
      CREATE INDEX index_custom_field_uuid ON pool_custom_field_options (custom_field_uuid)
    |sql}
;;

let add_custom_field_group_index =
  Sihl.Database.Migration.create_step
    ~label:"add custom field group index"
    {sql|
      CREATE INDEX index_custom_field_group_uuid ON pool_custom_fields (custom_field_group_uuid)
    |sql}
;;

let add_email_verification_index =
  Sihl.Database.Migration.create_step
    ~label:"add email verification index"
    {sql|
      CREATE INDEX index_user_uuid ON pool_email_verifications (user_uuid)
    |sql}
;;

let add_location_file_asset_index =
  Sihl.Database.Migration.create_step
    ~label:"add location file asset index"
    {sql|
      CREATE INDEX index_asset_id ON pool_location_file_mappings (asset_id)
    |sql}
;;

let add_location_file_location_index =
  Sihl.Database.Migration.create_step
    ~label:"add location file location index"
    {sql|
      CREATE INDEX index_location_id ON pool_location_file_mappings (location_id)
    |sql}
;;

let add_mailing_experiment_index =
  Sihl.Database.Migration.create_step
    ~label:"add location file location index"
    {sql|
      CREATE INDEX index_experiment_uuid ON pool_mailing (experiment_uuid)
    |sql}
;;

let add_session_experiment_index =
  Sihl.Database.Migration.create_step
    ~label:"add session experiment index"
    {sql|
      CREATE INDEX fk_pool_experiments ON pool_sessions (experiment_uuid)
    |sql}
;;

let migration () =
  Sihl.Database.Migration.(
    empty "202309180820"
    |> add_step add_assignment_session_index
    |> add_step add_assignment_contact_index
    |> add_step add_custom_field_answers_custom_field_uuid_index
    |> add_step add_custom_field_answers_entity_uuid_index
    |> add_step add_custom_field_options_index
    |> add_step add_custom_field_group_index
    |> add_step add_email_verification_index
    |> add_step add_location_file_asset_index
    |> add_step add_location_file_location_index
    |> add_step add_mailing_experiment_index
    |> add_step add_session_experiment_index)
;;

let add_system_event_logs_index =
  Sihl.Database.Migration.create_step
    ~label:"add system event logs index"
    {sql|
      CREATE INDEX index_system_event_uuid ON pool_system_event_logs (event_uuid)
    |sql}
;;

let add_tenant_logo_tenant_index =
  Sihl.Database.Migration.create_step
    ~label:"add tenant logo tenant index"
    {sql|
      CREATE INDEX index_tenant_uuid ON pool_tenant_logo_mappings (tenant_uuid)
    |sql}
;;

let add_tenant_logo_asset_index =
  Sihl.Database.Migration.create_step
    ~label:"add tenant logo asset index"
    {sql|
      CREATE INDEX index_asset_uuid ON pool_tenant_logo_mappings (asset_uuid)
    |sql}
;;

let migration_root () =
  Sihl.Database.Migration.(
    empty "202309180820"
    |> add_step add_system_event_logs_index
    |> add_step add_tenant_logo_tenant_index
    |> add_step add_tenant_logo_asset_index)
;;
