open CCFun
module Database = Database
module Dynparam = Database.Dynparam

let src = Logs.Src.create "experiment.repo"

let sql_select_columns =
  [ Entity.Id.sql_select_fragment ~field:"pool_experiments.uuid"
  ; "pool_experiments.title"
  ; "pool_experiments.public_title"
  ; "pool_experiments.internal_description"
  ; "pool_experiments.public_description"
  ; "pool_experiments.language"
  ; "pool_experiments.cost_center"
  ; "pool_experiments.contact_email"
  ; Entity.Id.sql_select_fragment ~field:"pool_experiments.smtp_auth_uuid"
  ; "pool_experiments.direct_registration_disabled"
  ; "pool_experiments.registration_disabled"
  ; "pool_experiments.allow_uninvited_signup"
  ; "pool_experiments.external_data_required"
  ; "pool_experiments.show_external_data_id_links"
  ; "pool_experiments.experiment_type"
  ; "pool_experiments.assignment_without_session"
  ; "pool_experiments.survey_url"
  ; "pool_experiments.email_session_reminder_lead_time"
  ; "pool_experiments.text_message_session_reminder_lead_time"
  ; "pool_experiments.invitation_reset_at"
  ; "pool_experiments.matcher_notification_sent"
  ; "pool_experiments.created_at"
  ; "pool_experiments.updated_at"
  ]
  @ Filter.Repo.sql_select_columns
  @ Organisational_unit.Repo.sql_select_columns
;;

let joins =
  {sql|
    LEFT JOIN pool_filter
      ON pool_filter.uuid = pool_experiments.filter_uuid
    LEFT JOIN pool_organisational_units
      ON pool_organisational_units.uuid = pool_experiments.organisational_unit_uuid
  |sql}
;;

let joins_tags =
  {sql|
    LEFT JOIN pool_tagging
      ON pool_tagging.model_uuid = pool_experiments.uuid
	  LEFT JOIN pool_tags
      ON pool_tags.uuid = pool_tagging.tag_uuid
  |sql}
;;

let find_request_sql ?(distinct = false) ?additional_joins ?(count = false) where_fragment
  =
  let columns =
    if count
    then "COUNT( DISTINCT pool_experiments.uuid )"
    else sql_select_columns |> CCString.concat ", "
  in
  let joins =
    additional_joins |> CCOption.map_or ~default:joins (Format.asprintf "%s\n%s" joins)
  in
  Format.asprintf
    {sql|SELECT %s %s FROM pool_experiments %s %s|sql}
    (if distinct && not count then "DISTINCT" else "")
    columns
    joins
    where_fragment
;;

let participation_history_sql additional_joins ?(count = false) where_fragment =
  let is_pending_col =
    {sql|
      EXISTS (
        SELECT
          1
        FROM
          pool_sessions
          INNER JOIN pool_assignments a ON a.session_uuid = pool_sessions.uuid
        WHERE
          a.contact_uuid = pool_assignments.contact_uuid
          AND a.canceled_at IS NULL
          AND a.marked_as_deleted = 0
          AND pool_sessions.experiment_uuid = pool_experiments.uuid
          AND(
            CASE WHEN pool_experiments.assignment_without_session = 1 THEN
              a.participated IS NULL
            ELSE
              pool_sessions.closed_at IS NULL
            END))
    |sql}
  in
  let columns =
    if count
    then "COUNT( DISTINCT pool_experiments.uuid )"
    else sql_select_columns @ [ is_pending_col ] |> CCString.concat ", "
  in
  let group_by = if count then "" else "GROUP BY pool_experiments.uuid" in
  let joins = Format.asprintf "%s\n%s\n%s" joins additional_joins joins_tags in
  Format.asprintf
    {sql|SELECT %s FROM pool_experiments %s %s %s |sql}
    columns
    joins
    where_fragment
    group_by
;;

module Sql = struct
  let default_order_by = "pool_experiments.created_at"

  let insert_sql =
    {sql|
      INSERT INTO pool_experiments (
        uuid,
        title,
        public_title,
        internal_description,
        public_description,
        language,
        cost_center,
        organisational_unit_uuid,
        filter_uuid,
        contact_email,
        smtp_auth_uuid,
        direct_registration_disabled,
        registration_disabled,
        allow_uninvited_signup,
        external_data_required,
        show_external_data_id_links,
        experiment_type,
        assignment_without_session,
        survey_url,
        email_session_reminder_lead_time,
        text_message_session_reminder_lead_time,
        invitation_reset_at,
        matcher_notification_sent
      ) VALUES (
        UNHEX(REPLACE(?, '-', '')),
        ?,
        ?,
        ?,
        ?,
        ?,
        ?,
        UNHEX(REPLACE(?, '-', '')),
        UNHEX(REPLACE(?, '-', '')),
        UNHEX(REPLACE(?, '-', '')),
        UNHEX(REPLACE(?, '-', '')),
        ?,
        ?,
        ?,
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
    |sql}
  ;;

  let insert_request =
    let open Caqti_request.Infix in
    insert_sql |> Repo_entity.Write.t ->. Caqti_type.unit
  ;;

  let insert pool = Database.exec pool insert_request

  let get_default_public_title pool =
    let request =
      let open Caqti_request.Infix in
      {sql|
        SELECT
          id
        FROM
          pool_experiments
        ORDER BY
          id DESC
        LIMIT 1
      |sql}
      |> Caqti_type.(unit ->! int)
    in
    let open Utils.Lwt_result.Infix in
    Database.find_opt pool request ()
    ||> CCOption.value ~default:0
    ||> fun max_id -> Format.asprintf "#%i" (max_id + 1)
  ;;

  let search_select =
    {sql|
        SELECT
          LOWER(CONCAT(
            SUBSTR(HEX(pool_experiments.uuid), 1, 8), '-',
            SUBSTR(HEX(pool_experiments.uuid), 9, 4), '-',
            SUBSTR(HEX(pool_experiments.uuid), 13, 4), '-',
            SUBSTR(HEX(pool_experiments.uuid), 17, 4), '-',
            SUBSTR(HEX(pool_experiments.uuid), 21)
          )),
          pool_experiments.title
        FROM pool_experiments
    |sql}
  ;;

  let validate_experiment_sql m = Format.asprintf " AND %s " m, Dynparam.empty

  let all pool =
    let open Caqti_request.Infix in
    let request =
      find_request_sql ~distinct:true "" |> Caqti_type.unit ->* Repo_entity.t
    in
    Database.collect pool request ()
  ;;

  let list_by_user ?query pool actor =
    let open CCFun.Infix in
    let dyn, sql, joins =
      Guard.Persistence.with_user_permission actor "pool_experiments.uuid" `Experiment
    in
    let select ?count =
      find_request_sql ?count ~distinct:true ~additional_joins:joins
      %> Format.asprintf "%s %s" sql
    in
    Query.collect_and_count pool query ~select ~dyn Repo_entity.t
  ;;

  let find_request =
    let open Caqti_request.Infix in
    {sql|
      WHERE pool_experiments.uuid = UNHEX(REPLACE(?, '-', ''))
    |sql}
    |> find_request_sql
    |> Caqti_type.string ->! Repo_entity.t
  ;;

  let find pool id =
    let open Utils.Lwt_result.Infix in
    Database.find_opt pool find_request (id |> Entity.Id.value)
    ||> CCOption.to_result Pool_message.(Error.NotFound Field.Experiment)
  ;;

  let find_of_session =
    let open Caqti_request.Infix in
    {sql|
      INNER JOIN pool_sessions
        ON pool_experiments.uuid = pool_sessions.experiment_uuid
      WHERE pool_sessions.uuid = UNHEX(REPLACE(?, '-', ''))
    |sql}
    |> find_request_sql
    |> Caqti_type.string ->! Repo_entity.t
  ;;

  let find_of_session pool id =
    let open Utils.Lwt_result.Infix in
    Database.find_opt pool find_of_session (id |> Pool_common.Id.value)
    ||> CCOption.to_result Pool_message.(Error.NotFound Field.Experiment)
  ;;

  let find_of_mailing =
    let open Caqti_request.Infix in
    {sql|
      WHERE pool_experiments.uuid = (SELECT experiment_uuid FROM pool_mailing WHERE uuid = UNHEX(REPLACE(?, '-', '')) )
    |sql}
    |> find_request_sql
    |> Caqti_type.string ->! Repo_entity.t
  ;;

  let find_of_mailing pool id =
    let open Utils.Lwt_result.Infix in
    Database.find_opt pool find_of_mailing (id |> Pool_common.Id.value)
    ||> CCOption.to_result Pool_message.(Error.NotFound Field.Experiment)
  ;;

  let session_count_request =
    let open Caqti_request.Infix in
    {sql|
      SELECT COUNT(1) FROM pool_sessions WHERE experiment_uuid = UNHEX(REPLACE(?, '-', ''))
    |sql}
    |> Caqti_type.(string ->! int)
  ;;

  let session_count pool id =
    Database.find pool session_count_request (id |> Pool_common.Id.value)
  ;;

  let update_request =
    let open Caqti_request.Infix in
    {sql|
      UPDATE pool_experiments
      SET
        title = $2,
        public_title = $3,
        internal_description = $4,
        public_description = $5,
        language = $6,
        cost_center = $7,
        organisational_unit_uuid = UNHEX(REPLACE($8, '-', '')),
        filter_uuid = UNHEX(REPLACE($9, '-', '')),
        contact_email = $10,
        smtp_auth_uuid = UNHEX(REPLACE($11, '-', '')),
        direct_registration_disabled = $12,
        registration_disabled = $13,
        allow_uninvited_signup = $14,
        external_data_required = $15,
        show_external_data_id_links = $16,
        experiment_type = $17,
        assignment_without_session = $18,
        survey_url = $19,
        email_session_reminder_lead_time = $20,
        text_message_session_reminder_lead_time = $21,
        invitation_reset_at = $22,
        matcher_notification_sent = $23
      WHERE
        uuid = UNHEX(REPLACE($1, '-', ''))
    |sql}
    |> Repo_entity.Write.t ->. Caqti_type.unit
  ;;

  let update pool = Database.exec pool update_request

  let delete_request =
    let open Caqti_request.Infix in
    {sql|
      DELETE FROM pool_experiments
      WHERE uuid = UNHEX(REPLACE($1, '-', ''))
    |sql}
    |> Caqti_type.(string ->. unit)
  ;;

  let delete pool id = Database.exec pool delete_request (id |> Entity.Id.value)

  let search_request ?conditions ?joins ~limit () =
    let default_contidion = "pool_experiments.title LIKE ?" in
    let joined_select =
      CCOption.map_or ~default:search_select (Format.asprintf "%s %s" search_select) joins
    in
    let where =
      CCOption.map_or
        ~default:default_contidion
        (Format.asprintf "%s AND %s" default_contidion)
        conditions
    in
    Format.asprintf "%s WHERE %s LIMIT %i" joined_select where limit
  ;;

  let search ?conditions ?(dyn = Dynparam.empty) ?exclude ?joins ?(limit = 20) pool query =
    let open Caqti_request.Infix in
    let exclude_ids = Database.exclude_ids "pool_experiments.uuid" Entity.Id.value in
    let dyn = Dynparam.(dyn |> add Caqti_type.string ("%" ^ query ^ "%")) in
    let dyn, exclude =
      exclude |> CCOption.map_or ~default:(dyn, None) (exclude_ids dyn)
    in
    let conditions =
      [ conditions; exclude ]
      |> CCList.filter_map CCFun.id
      |> function
      | [] -> None
      | conditions -> conditions |> CCString.concat " AND " |> CCOption.return
    in
    let (Dynparam.Pack (pt, pv)) = dyn in
    let request =
      search_request ?conditions ?joins ~limit ()
      |> pt ->* Repo_entity.(Caqti_type.t2 Id.t Title.t)
    in
    Database.collect pool request pv
  ;;

  let search_multiple_by_id_request ids =
    Format.asprintf
      {sql|
        %s
        WHERE pool_experiments.uuid in ( %s )
      |sql}
      search_select
      (CCList.map (fun _ -> Format.asprintf "UNHEX(REPLACE(?, '-', ''))") ids
       |> CCString.concat ",")
  ;;

  let search_multiple_by_id pool ids =
    let open Caqti_request.Infix in
    match ids with
    | [] -> Lwt.return []
    | ids ->
      let dyn =
        CCList.fold_left
          (fun dyn id ->
             dyn |> Dynparam.add Caqti_type.string (id |> Pool_common.Id.value))
          Dynparam.empty
          ids
      in
      let (Dynparam.Pack (pt, pv)) = dyn in
      let request =
        search_multiple_by_id_request ids
        |> pt ->* Caqti_type.(Repo_entity.(t2 Repo_entity.Id.t Title.t))
      in
      Database.collect pool request pv
  ;;

  let find_all_ids_of_contact_id_request =
    let open Caqti_request.Infix in
    {sql|
        SELECT
          LOWER(CONCAT(
            SUBSTR(HEX(pool_experiments.uuid), 1, 8), '-',
            SUBSTR(HEX(pool_experiments.uuid), 9, 4), '-',
            SUBSTR(HEX(pool_experiments.uuid), 13, 4), '-',
            SUBSTR(HEX(pool_experiments.uuid), 17, 4), '-',
            SUBSTR(HEX(pool_experiments.uuid), 21)
          ))
        FROM pool_experiments
        INNER JOIN pool_sessions
          ON pool_experiments.uuid = pool_sessions.experiment_uuid
        INNER JOIN pool_assignments
          ON pool_sessions.uuid = pool_assignments.session_uuid
        WHERE pool_assignments.contact_uuid = UNHEX(REPLACE(?, '-', ''))
      |sql}
    |> Pool_common.Repo.Id.t ->* Repo_entity.Id.t
  ;;

  let find_all_ids_of_contact_id pool id =
    Database.collect pool find_all_ids_of_contact_id_request (Contact.Id.to_common id)
  ;;

  let find_to_enroll_directly_request where =
    let open Caqti_request.Infix in
    Format.asprintf
      {sql|
        SELECT DISTINCT
          LOWER(CONCAT(
            SUBSTR(HEX(pool_experiments.uuid), 1, 8), '-',
            SUBSTR(HEX(pool_experiments.uuid), 9, 4), '-',
            SUBSTR(HEX(pool_experiments.uuid), 13, 4), '-',
            SUBSTR(HEX(pool_experiments.uuid), 17, 4), '-',
            SUBSTR(HEX(pool_experiments.uuid), 21)
          )),
          pool_experiments.title,
          pool_experiments.public_title,
          pool_filter.query,
          pool_experiments.direct_registration_disabled,
          pool_experiments.registration_disabled,
          COUNT(pool_sessions.uuid) > 0,
          EXISTS(
            SELECT TRUE FROM pool_sessions
            LEFT JOIN pool_assignments
              ON pool_assignments.session_uuid = pool_sessions.uuid
              AND pool_assignments.contact_uuid = UNHEX(REPLACE($2, '-', ''))
            WHERE pool_sessions.experiment_uuid = pool_experiments.uuid
              AND pool_assignments.canceled_at IS NULL
              AND pool_assignments.marked_as_deleted = 0
          )
        FROM
          pool_experiments
          LEFT JOIN pool_filter ON pool_filter.uuid = pool_experiments.filter_uuid
          LEFT JOIN pool_sessions ON pool_sessions.experiment_uuid = pool_experiments.uuid
            AND pool_sessions.closed_at IS NULL
            AND pool_sessions.canceled_at IS NULL
            AND pool_sessions.max_participants + pool_sessions.overbook >
            (SELECT COUNT(*) FROM pool_assignments
             WHERE pool_assignments.session_uuid = pool_sessions.uuid
               AND pool_assignments.canceled_at IS NULL
               AND pool_assignments.marked_as_deleted = 0)
          WHERE
            (pool_experiments.title LIKE $1
            OR pool_experiments.public_title LIKE $1)
            AND pool_experiments.assignment_without_session = 0
            %s
          GROUP BY
            pool_experiments.uuid
          ORDER BY
            pool_experiments.created_at DESC
          LIMIT 5
        |sql}
      (where |> CCOption.map_or ~default:"" (fun where -> "AND " ^ where))
    |> Caqti_type.(t2 string Pool_common.Repo.Id.t) ->* Repo_entity.DirectEnrollment.t
  ;;

  let find_to_enroll_directly ?actor pool contact ~query =
    let open Utils.Lwt_result.Infix in
    let open Entity in
    let checks = [ Format.asprintf "pool_experiments.uuid IN %s" ] in
    let%lwt where =
      let open Guard in
      let permission = CCOption.map (const Permission.Create) actor in
      create_where ?actor ?permission ~checks pool `Assignment
    in
    Database.collect
      pool
      (find_to_enroll_directly_request where)
      ("%" ^ query ^ "%", Contact.(contact |> id |> Id.to_common))
    >|> Lwt_list.map_s (fun ({ DirectEnrollment.filter; _ } as experiment) ->
      let%lwt matches_filter =
        match filter with
        | None -> Lwt.return_true
        | Some filter -> Filter.contact_matches_filter pool filter contact
      in
      Lwt.return DirectEnrollment.{ experiment with matches_filter })
  ;;

  let contact_is_enrolled_request =
    let open Caqti_request.Infix in
    {sql|
    SELECT
      EXISTS (
        SELECT
          1
        FROM
          pool_assignments
          INNER JOIN pool_sessions ON pool_assignments.session_uuid = pool_sessions.uuid
          INNER JOIN pool_experiments ON pool_experiments.uuid = pool_sessions.experiment_uuid
        WHERE
          pool_sessions.canceled_at IS NULL
          AND pool_assignments.marked_as_deleted = 0
          AND pool_experiments.uuid = UNHEX(REPLACE(?, '-', ''))
          AND pool_assignments.contact_uuid = UNHEX(REPLACE(?, '-', '')))
    |sql}
    |> Caqti_type.(t2 Repo_entity.Id.t Contact.Repo.Id.t ->! bool)
  ;;

  let contact_is_enrolled pool experiment_id contact_id =
    Database.find pool contact_is_enrolled_request (experiment_id, contact_id)
  ;;

  let find_targets_grantable_by_target ?exclude database_label target_id role query =
    let joins =
      {sql|
      LEFT JOIN guardian_actor_role_targets t ON t.target_uuid = pool_experiments.uuid
        AND t.actor_uuid = UNHEX(REPLACE(?, '-', ''))
        AND t.role = ?
    |sql}
    in
    let conditions = "(t.role IS NULL OR t.mark_as_deleted IS NOT NULL)" in
    let dyn =
      let open Pool_common in
      Dynparam.(
        empty
        |> add Repo.Id.t (Guard.Uuid.Target.to_string target_id |> Id.of_string)
        |> add Caqti_type.string Role.Role.(show role))
    in
    search ~conditions ~joins ~dyn ?exclude database_label query
  ;;

  let participation_history_where ?(dyn = Dynparam.empty) ~only_closed contact_id =
    let joins =
      {sql|
        INNER JOIN pool_sessions ON pool_sessions.experiment_uuid = pool_experiments.uuid
          AND pool_sessions.canceled_at IS NULL
        INNER JOIN pool_assignments ON pool_assignments.session_uuid = pool_sessions.uuid
          AND pool_assignments.canceled_at IS NULL
          AND pool_assignments.marked_as_deleted = 0
      |sql}
    in
    let where =
      let only_closed_condition =
        {sql|
          CASE WHEN pool_experiments.assignment_without_session = 1 THEN
            pool_assignments.participated = 1
          ELSE
            pool_sessions.closed_at IS NOT NULL
          END
        |sql}
      in
      Format.asprintf
        {sql|
            pool_assignments.contact_uuid = UNHEX(REPLACE(?, '-', ''))
            %s
        |sql}
        (if only_closed then Format.asprintf "AND (%s)" only_closed_condition else "")
    in
    where, Dynparam.(dyn |> add Contact.Repo.Id.t contact_id), joins
  ;;

  let query_participation_history_by_contact ?query pool contact =
    let where, dyn, additional_joins =
      Contact.id contact |> participation_history_where ~only_closed:false
    in
    let select = participation_history_sql additional_joins in
    Caqti_type.(t2 Repo_entity.t bool)
    |> Query.collect_and_count pool query ~select ~where ~dyn
  ;;

  let count_invitations_request ?(by_count = false) () =
    let base =
      {sql|
      SELECT COUNT(1)
      FROM pool_invitations
      WHERE experiment_uuid = UNHEX(REPLACE(?, '-', ''))
    |sql}
    in
    match by_count with
    | false -> base
    | true -> Format.asprintf "%s \n %s" base "AND send_count = ?"
  ;;

  let total_invitation_count_by_experiment pool experiment_id =
    let open Caqti_request.Infix in
    Database.find
      pool
      (count_invitations_request () |> Caqti_type.(string ->! int))
      (Entity.Id.value experiment_id)
  ;;
end

let find = Sql.find
let all = Sql.all
let find_all_ids_of_contact_id = Sql.find_all_ids_of_contact_id
let find_of_session = Sql.find_of_session
let find_of_mailing = Sql.find_of_mailing
let session_count = Sql.session_count
let insert = Sql.insert
let update = Sql.update
let delete = Sql.delete
let search = Sql.search
let search_multiple_by_id = Sql.search_multiple_by_id
let find_to_enroll_directly = Sql.find_to_enroll_directly
let contact_is_enrolled = Sql.contact_is_enrolled
let find_targets_grantable_by_target = Sql.find_targets_grantable_by_target
