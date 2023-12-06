open Containers
open Caqti_request.Infix

let select pool subquery subquery_type =
  let query =
    [%string
      {sql|
      SELECT
        %{ Model.sql_select_fragment }
      FROM pool_experiments
      LEFT JOIN pool_filter
        ON pool_filter.uuid = pool_experiments.filter_uuid
      LEFT JOIN pool_organisational_units
        ON pool_organisational_units.uuid = pool_experiments.organisational_unit_uuid
      %{ subquery }
    |sql}]
    |> subquery_type ->! Model.schema
  in
  Utils.Database.find_opt (Pool_database.Label.value pool) query
;;

let find pool id =
  select
    pool
    {sql| WHERE pool_experiments.uuid = %{ Id.sql_value_fragment } |sql}
    Caqti_type.string
    (Pool_common.Id.to_string id)
;;

let insert pool experiment =
  let module Id = Pool_common.Id in
  let insert =
    [%string
      {sql|
      INSERT INTO pool_experiments (
        uuid,
        title,
        public_title,
        description,
        cost_center,
        organisational_unit_uuid,
        filter_uuid,
        contact_person_uuid,
        smtp_auth_uuid,
        direct_registration_disabled,
        registration_disabled,
        allow_uninvited_signup,
        external_data_required,
        show_external_data_id_links,
        experiment_type,
        email_session_reminder_lead_time,
        text_message_session_reminder_lead_time,
        invitation_reset_at
      ) VALUES (
        %{ Id.sql_value_fragment "?" }
        ?,
        ?,
        ?,
        ?,
        %{ Id.sql_value_fragment "?" }
        %{ Id.sql_value_fragment "?" }
        %{ Id.sql_value_fragment "?" }
        %{ Id.sql_value_fragment "?" }
        ?,
        ?,
        ?,
        ?,
        ?,
        ?,
        ?,
        ?,
        ?
      )
    |sql}]
    |> Model.schema ->. Caqti_type.unit
  in
  let autofill_public_title =
    [%string
      {sql|
      UPDATE pool_experiments
      SET
        public_title = CONCAT('#', id)
      WHERE
        uuid = %{ Id.sql_value_fragment "$1" }
      AND
        public_title = $2
    |sql}]
    |> Caqti_type.(t2 Pool_common.Repo.Id.t string ->. unit)
  in
  let open Utils.Database in
  exec_as_transaction
    (Pool_database.Label.value pool)
    [ exec_query insert experiment
    ; exec_query
        autofill_public_title
        (Model.id experiment, Model.public_title_placeholder)
    ]
;;
