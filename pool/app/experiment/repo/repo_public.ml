module RepoEntity = Repo_entity
module Dynparam = Database.Dynparam

let sql_select_columns =
  [ Entity.Id.sql_select_fragment ~field:"pool_experiments.uuid"
  ; "pool_experiments.public_title"
  ; "pool_experiments.public_description"
  ; "pool_experiments.language"
  ; "pool_experiments.direct_registration_disabled"
  ; "pool_experiments.experiment_type"
  ; Entity.Id.sql_select_fragment ~field:"pool_experiments.smtp_auth_uuid"
  ; "pool_experiments.assignment_without_session"
  ; "pool_experiments.survey_url"
  ]
;;

let select_from_experiments_sql ?(distinct = false) where_fragment =
  let select_from =
    Format.asprintf
      {sql|
        SELECT %s %s
        FROM pool_experiments
      |sql}
      (if distinct then "DISTINCT" else "")
      (CCString.concat "," sql_select_columns)
  in
  Format.asprintf "%s %s" select_from where_fragment
;;

let select_upcoming_sql ?(count = false) where_fragment =
  let select =
    if count
    then "COUNT(DISTINCT pool_experiments.id)"
    else CCString.concat "," sql_select_columns |> Format.asprintf "DISTINCT %s"
  in
  Format.asprintf
    {sql|
      SELECT %s
      FROM pool_experiments
      INNER JOIN pool_sessions
        ON pool_sessions.experiment_uuid = pool_experiments.uuid
        AND pool_sessions.canceled_at IS NULL
      LEFT OUTER JOIN pool_invitations
        ON pool_invitations.contact_uuid = UNHEX(REPLACE(?, '-', ''))
        AND pool_experiments.uuid = pool_invitations.experiment_uuid
      LEFT OUTER JOIN pool_waiting_list
        ON pool_waiting_list.contact_uuid = UNHEX(REPLACE(?, '-', ''))
        AND pool_experiments.uuid = pool_waiting_list.experiment_uuid
      %s
    |sql}
    select
    where_fragment
;;

let pool_sessions_inner_join =
  {sql|
    INNER JOIN pool_sessions
    ON pool_sessions.experiment_uuid = pool_experiments.uuid
  |sql}
;;

let pool_invitations_left_join =
  {sql|
      LEFT OUTER JOIN pool_invitations
      ON pool_invitations.contact_uuid = UNHEX(REPLACE($1, '-', ''))
      AND pool_experiments.uuid = pool_invitations.experiment_uuid
    |sql}
;;

let condition_registration_not_disabled = "pool_experiments.registration_disabled = 0"

let condition_allow_uninvited_signup =
  {sql|
    pool_experiments.allow_uninvited_signup = 1
  |sql}
;;

let assignments_base_condition ~require_participated =
  let condition =
    if require_participated then "AND pool_assignments.participated = 1" else ""
  in
  Format.asprintf
    {sql|
    EXISTS (
      SELECT
        1
      FROM
        pool_assignments
        INNER JOIN pool_sessions ON pool_assignments.session_uuid = pool_sessions.uuid
      WHERE
        pool_sessions.experiment_uuid = pool_experiments.uuid
        AND pool_assignments.contact_uuid = UNHEX(REPLACE($1, '-', ''))
        AND pool_assignments.marked_as_deleted = 0
        AND pool_sessions.canceled_at IS NULL
        %s)
    |sql}
    condition
;;

let condition_assigned = assignments_base_condition ~require_participated:false
let condition_participated = assignments_base_condition ~require_participated:true

let condition_is_invited =
  {sql| pool_invitations.contact_uuid = UNHEX(REPLACE($1, '-', '')) |sql}
;;

let upcoming_where experiment_type =
  let onsite_session_exists =
    {sql|
      (pool_sessions.start > NOW()
        AND
      pool_sessions.canceled_at IS NULL)
    |sql}
  in
  let timewindow_exists =
    {sql|
      (DATE_ADD(pool_sessions.start, INTERVAL pool_sessions.duration SECOND) > NOW()
        AND
      pool_sessions.canceled_at IS NULL)
    |sql}
  in
  let experiment_type_condition, session_condition =
    let type_condition =
      Format.asprintf {sql| pool_experiments.assignment_without_session = %s |sql}
    in
    match experiment_type with
    | `Online -> type_condition "1", timewindow_exists
    | `OnSite -> type_condition "0", onsite_session_exists
  in
  let inivitation_exists = "pool_invitations.contact_uuid IS NOT NULL" in
  let not_assigned =
    let subquery =
      {sql|
        SELECT
          1
        FROM
          pool_assignments
          INNER JOIN pool_sessions ON pool_assignments.session_uuid = pool_sessions.uuid
        WHERE
          pool_sessions.experiment_uuid = pool_experiments.uuid
          AND pool_assignments.contact_uuid = UNHEX(REPLACE(?, '-', ''))
          AND pool_assignments.marked_as_deleted = 0
          AND pool_sessions.canceled_at IS NULL
      |sql}
    in
    match experiment_type with
    | `OnSite -> Format.asprintf "NOT EXISTS (%s)" subquery
    | `Online ->
      Format.asprintf
        "NOT EXISTS (%s AND pool_assignments.participated IS NOT NULL)"
        subquery
  in
  let not_on_waitinglist = "pool_waiting_list.uuid IS NULL" in
  Format.asprintf
    "%s AND (%s OR %s) AND %s AND %s AND %s AND %s"
    experiment_type_condition
    inivitation_exists
    condition_allow_uninvited_signup
    not_assigned
    condition_registration_not_disabled
    not_on_waitinglist
    session_condition
;;

let find_upcoming_to_register_request experiment_type () =
  let open Caqti_request.Infix in
  let onsite_session_exists =
    {sql|
      (pool_sessions.start > NOW()
        AND
      pool_sessions.canceled_at IS NULL)
    |sql}
  in
  let timewindow_exists =
    {sql|
      (DATE_ADD(pool_sessions.start, INTERVAL pool_sessions.duration SECOND) > NOW()
        AND
      pool_sessions.canceled_at IS NULL)
    |sql}
  in
  let experiment_type, session_condition, assignment_condition, order_by =
    let type_condition =
      Format.asprintf {sql| pool_experiments.assignment_without_session = %s |sql}
    in
    match experiment_type with
    | `Online ->
      ( type_condition "1"
      , timewindow_exists
      , condition_participated
      , "ORDER BY pool_sessions.start" )
    | `OnSite -> type_condition "0", onsite_session_exists, condition_assigned, ""
  in
  let not_on_waitinglist =
    {sql|
    NOT EXISTS (
      SELECT
        1
      FROM pool_waiting_list
      WHERE
        pool_waiting_list.contact_uuid = UNHEX(REPLACE($1, '-', ''))
      AND
        pool_waiting_list.experiment_uuid = pool_experiments.uuid
      AND
        pool_waiting_list.marked_as_deleted = 0
      )
      |sql}
  in
  (* TODO: make those subqueries joins (#2248) *)
  Format.asprintf
    "%s %s WHERE %s AND %s AND %s AND %s AND (%s OR %s) AND %s GROUP BY \
     pool_experiments.uuid %s"
    pool_sessions_inner_join
    pool_invitations_left_join
    experiment_type
    ("NOT " ^ assignment_condition)
    not_on_waitinglist
    condition_registration_not_disabled
    condition_allow_uninvited_signup
    condition_is_invited
    session_condition
    order_by
  |> Repo.find_request_sql
  |> Contact.Repo.Id.t ->* RepoEntity.t
;;

let filter_of_public_request =
  let open Caqti_request.Infix in
  Filter.Repo.sql_select_columns
  |> CCString.concat ","
  |> Format.asprintf
       {sql|
          SELECT %s FROM pool_filter WHERE uuid = (SELECT filter_uuid FROM pool_experiments WHERE uuid = UNHEX(REPLACE($1, '-', '')))
        |sql}
  |> Repo_entity.Id.t ->? Filter.Repo.t
;;

let find_upcoming_to_register pool contact experiment_type =
  let open Utils.Lwt_result.Infix in
  Database.collect
    pool
    (find_upcoming_to_register_request experiment_type ())
    (Contact.id contact)
  (* TODO: This has to be made superfluous by a background job (#164) *)
  >|> Lwt_list.filter_s (fun experiment ->
    Entity.contact_meets_criteria pool experiment contact)
  ||> CCList.map Entity.to_public
;;

let find_upcoming pool query contact experiment_type =
  let query =
    match query with
    | `Dashboard limit ->
      let open Query in
      let pagination = Pagination.create ~limit ~page:0 () in
      create ~pagination ()
    | `Query query -> query
  in
  (* Join with waiting list, invitation and assignments tables *)
  let dyn =
    let open Contact in
    let id = id contact in
    Dynparam.(empty |> add Repo.Id.t id |> add Repo.Id.t id |> add Repo.Id.t id)
  in
  let where = upcoming_where experiment_type in
  Query.collect_and_count
    pool
    (Some query)
    ~select:select_upcoming_sql
    ~dyn
    ~where
    Repo_entity.Public.t
;;

let find_pending_waitinglists_by_contact_request =
  let open Caqti_request.Infix in
  let join =
    {sql|
    INNER JOIN pool_waiting_list
    ON
      pool_waiting_list.experiment_uuid = pool_experiments.uuid
    AND
      pool_waiting_list.marked_as_deleted = 0
    AND
      pool_waiting_list.contact_uuid = UNHEX(REPLACE($1, '-', ''))
    WHERE NOT EXISTS (
      SELECT 1 FROM pool_assignments
      INNER JOIN pool_sessions
      ON
        pool_sessions.uuid = pool_assignments.session_uuid
      AND
        pool_sessions.canceled_at IS NULL
      WHERE
        pool_sessions.experiment_uuid = pool_experiments.uuid
      AND
        pool_assignments.contact_uuid = UNHEX(REPLACE($1, '-', ''))
      AND
        pool_assignments.marked_as_deleted = 0)
  |sql}
  in
  join
  |> select_from_experiments_sql ~distinct:true
  |> Contact.Repo.Id.t ->* RepoEntity.Public.t
;;

let find_pending_waitinglists_by_contact pool contact =
  Database.collect pool find_pending_waitinglists_by_contact_request (Contact.id contact)
;;

let where_contact_can_access =
  let id_fragment = "pool_experiments.uuid = UNHEX(REPLACE($2, '-', ''))" in
  let waiting_list_join =
    {sql|
    LEFT OUTER JOIN pool_waiting_list
      ON pool_waiting_list.contact_uuid = UNHEX(REPLACE($1, '-', ''))
      AND pool_experiments.uuid = pool_waiting_list.experiment_uuid
      AND pool_waiting_list.marked_as_deleted = 0
  |sql}
  in
  let waiting_list_exists =
    {sql|
      (pool_waiting_list.contact_uuid = UNHEX(REPLACE($1, '-', ''))
      AND pool_waiting_list.marked_as_deleted = 0)
      |sql}
  in
  Format.asprintf
    "%s %s WHERE %s AND (%s OR %s OR (%s AND %s) OR %s)"
    waiting_list_join
    pool_invitations_left_join
    id_fragment
    condition_allow_uninvited_signup
    condition_assigned
    condition_registration_not_disabled
    condition_is_invited
    waiting_list_exists
;;

let find_request =
  let open Caqti_request.Infix in
  where_contact_can_access
  |> select_from_experiments_sql
  |> Caqti_type.(t2 Contact.Repo.Id.t RepoEntity.Id.t) ->! RepoEntity.Public.t
;;

let find pool id contact =
  let open Utils.Lwt_result.Infix in
  Database.find_opt pool find_request (Contact.id contact, id)
  ||> CCOption.to_result Pool_message.(Error.NotFound Field.Experiment)
;;

let find_full_by_contact_request =
  let open Caqti_request.Infix in
  where_contact_can_access
  |> Repo.find_request_sql
  |> Caqti_type.(t2 Contact.Repo.Id.t RepoEntity.Id.t) ->! RepoEntity.t
;;

let find_full_by_contact pool id contact =
  let open Utils.Lwt_result.Infix in
  Database.find_opt pool find_full_by_contact_request (Contact.id contact, id)
  ||> CCOption.to_result Pool_message.(Error.NotFound Field.Experiment)
;;

let contact_matches_filter database_label public contact =
  let open Utils.Lwt_result.Infix in
  Database.find_opt database_label filter_of_public_request (Entity.Public.id public)
  >|> function
  | None -> Lwt.return true
  | Some { Filter.query; _ } -> Filter.contact_matches_filter database_label query contact
;;
