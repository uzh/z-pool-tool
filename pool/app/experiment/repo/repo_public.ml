module RepoEntity = Repo_entity

let select_from_experiments_sql ?(distinct = false) where_fragment =
  let select_from =
    Format.asprintf
      {sql|
        SELECT %s
          LOWER(CONCAT(
            SUBSTR(HEX(pool_experiments.uuid), 1, 8), '-',
            SUBSTR(HEX(pool_experiments.uuid), 9, 4), '-',
            SUBSTR(HEX(pool_experiments.uuid), 13, 4), '-',
            SUBSTR(HEX(pool_experiments.uuid), 17, 4), '-',
            SUBSTR(HEX(pool_experiments.uuid), 21)
          )),
          pool_experiments.public_title,
          pool_experiments.description,
          pool_experiments.language,
          pool_experiments.direct_registration_disabled,
          pool_experiments.experiment_type,
          LOWER(CONCAT(
            SUBSTR(HEX(pool_experiments.smtp_auth_uuid), 1, 8), '-',
            SUBSTR(HEX(pool_experiments.smtp_auth_uuid), 9, 4), '-',
            SUBSTR(HEX(pool_experiments.smtp_auth_uuid), 13, 4), '-',
            SUBSTR(HEX(pool_experiments.smtp_auth_uuid), 17, 4), '-',
            SUBSTR(HEX(pool_experiments.smtp_auth_uuid), 21)
          ))
        FROM pool_experiments
      |sql}
      (if distinct then "DISTINCT" else "")
  in
  Format.asprintf "%s %s" select_from where_fragment
;;

let pool_invitations_left_join =
  {sql|
      LEFT OUTER JOIN pool_invitations
      ON pool_invitations.contact_uuid = UNHEX(REPLACE($1, '-', ''))
      AND pool_experiments.uuid = pool_invitations.experiment_uuid
    |sql}
;;

let condition_registration_not_disabled =
  "pool_experiments.registration_disabled = 0"
;;

let condition_allow_uninvited_signup =
  Format.asprintf
    {sql|
      pool_experiments.allow_uninvited_signup = 1
    |sql}
;;

let find_all_public_by_contact_request ?(has_session = false) () =
  let open Caqti_request.Infix in
  let session_exists =
    match has_session with
    | false -> ""
    | true ->
      {sql|
      AND EXISTS (SELECT
        1
      FROM
        pool_sessions
      WHERE
        pool_sessions.experiment_uuid = pool_experiments.uuid
      AND pool_sessions.start > NOW())
    |sql}
  in
  let not_assigned =
    {sql|
    NOT EXISTS (
      SELECT
        1
      FROM
        pool_assignments
        INNER JOIN pool_sessions ON pool_assignments.session_uuid = pool_sessions.uuid
      WHERE
        pool_sessions.experiment_uuid = pool_experiments.uuid
        AND pool_assignments.contact_uuid = UNHEX(REPLACE($1, '-', ''))
        AND pool_assignments.marked_as_deleted = 0
        AND pool_sessions.canceled_at IS NULL)
      |sql}
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
      )
      |sql}
  in
  let is_invited =
    {sql| pool_invitations.contact_uuid = UNHEX(REPLACE($1, '-', '')) |sql}
  in
  Format.asprintf
    "%s WHERE %s AND %s AND %s AND (%s OR %s) %s"
    pool_invitations_left_join
    not_assigned
    not_on_waitinglist
    condition_registration_not_disabled
    condition_allow_uninvited_signup
    is_invited
    session_exists
  |> Repo.find_request_sql
  |> Pool_common.Repo.Id.t ->* RepoEntity.t
;;

let find_all_public_by_contact ?has_session pool contact =
  let open Utils.Lwt_result.Infix in
  Utils.Database.collect
    (Pool_database.Label.value pool)
    (find_all_public_by_contact_request ?has_session ())
    (Contact.id contact)
  (* TODO: This has to be made superfluous by a background job (#164) *)
  >|> Lwt_list.filter_s
        Filter.(
          fun { Entity.filter; _ } ->
            filter
            |> CCOption.map_or ~default:Lwt.return_true (fun { query; _ } ->
              contact_matches_filter pool query contact))
  ||> CCList.map Entity.to_public
;;

let find_pending_waitinglists_by_contact_request =
  let open Caqti_request.Infix in
  let join =
    {sql|
    INNER JOIN pool_waiting_list
    ON
      pool_waiting_list.experiment_uuid = pool_experiments.uuid
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
  |> Pool_common.Repo.Id.t ->* RepoEntity.Public.t
;;

let find_pending_waitinglists_by_contact pool contact =
  Utils.Database.collect
    (Pool_database.Label.value pool)
    find_pending_waitinglists_by_contact_request
    (Contact.id contact)
;;

let find_past_experiments_by_contact_request =
  let open Caqti_request.Infix in
  {sql|
    INNER JOIN pool_sessions ON pool_sessions.experiment_uuid = pool_experiments.uuid
      AND pool_sessions.closed_at IS NOT NULL
    INNER JOIN pool_assignments ON pool_assignments.session_uuid = pool_sessions.uuid
      AND pool_assignments.canceled_at IS NULL
    WHERE
      pool_assignments.contact_uuid = UNHEX(REPLACE($1, '-', ''))
  |sql}
  |> select_from_experiments_sql ~distinct:true
  |> Pool_common.Repo.Id.t ->* RepoEntity.Public.t
;;

let find_past_experiments_by_contact pool contact =
  Utils.Database.collect
    (Pool_database.Label.value pool)
    find_past_experiments_by_contact_request
    (Contact.id contact)
;;

let where_contact_can_access =
  let id_fragment = "pool_experiments.uuid = UNHEX(REPLACE($2, '-', ''))" in
  let assignment_exists =
    {sql|
    EXISTS (
      SELECT
        1
      FROM
        pool_assignments
        INNER JOIN pool_sessions ON pool_assignments.session_uuid = pool_sessions.uuid
          AND pool_sessions.experiment_uuid = UNHEX(REPLACE($2, '-', ''))
      WHERE
        pool_assignments.contact_uuid = UNHEX(REPLACE($1, '-', ''))
        AND pool_assignments.canceled_at IS NULL)
  |sql}
  in
  let invitation_uuid =
    {sql| pool_invitations.contact_uuid = UNHEX(REPLACE($1, '-', '')) |sql}
  in
  Format.asprintf
    "%s WHERE %s AND (%s OR %s OR %s)"
    pool_invitations_left_join
    id_fragment
    condition_allow_uninvited_signup
    assignment_exists
    invitation_uuid
;;

let find_request =
  let open Caqti_request.Infix in
  where_contact_can_access
  |> select_from_experiments_sql
  |> Caqti_type.(t2 string string) ->! RepoEntity.Public.t
;;

let find pool id contact =
  let open Utils.Lwt_result.Infix in
  Utils.Database.find_opt
    (Pool_database.Label.value pool)
    find_request
    Pool_common.Id.(Contact.id contact |> value, id |> value)
  ||> CCOption.to_result Pool_common.Message.(NotFound Field.Experiment)
;;

let find_full_by_contact_request =
  let open Caqti_request.Infix in
  where_contact_can_access
  |> Repo.find_request_sql
  |> Caqti_type.(t2 string string) ->! RepoEntity.t
;;

let find_full_by_contact pool id contact =
  let open Utils.Lwt_result.Infix in
  Utils.Database.find_opt
    (Pool_database.Label.value pool)
    find_full_by_contact_request
    Pool_common.Id.(Contact.id contact |> value, id |> value)
  ||> CCOption.to_result Pool_common.Message.(NotFound Field.Experiment)
;;
