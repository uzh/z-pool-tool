let registration_disabled_request =
  let open Caqti_request.Infix in
  {sql|
      SELECT
        registration_disabled
      FROM
        pool_experiments
      WHERE
        uuid = UNHEX(REPLACE(?, '-', ''))
    |sql}
  |> Caqti_type.(Repo_entity.Id.t ->! bool)
;;

let has_open_sessions_request =
  let open Caqti_request.Infix in
  {sql|
    SELECT
      COUNT(s.uuid) > 1
    FROM
      pool_sessions s
      LEFT JOIN (
        SELECT
          session_uuid,
          COUNT(*) AS count
        FROM
          pool_assignments
        GROUP BY
          session_uuid) assignments ON s.uuid = assignments.session_uuid
    WHERE
      s.experiment_uuid = UNHEX(REPLACE(?, '-', ''))
      AND canceled_at IS NULL
      AND closed_at IS NULL
      AND 
        (assignments.count IS NULL 
          OR 
        assignments.count < s.max_participants + s.overbook)
    |sql}
  |> Caqti_type.(Repo_entity.Id.t ->! bool)
;;

let registration_possible pool id =
  let open Utils.Lwt_result.Infix in
  Utils.Database.find
    (Pool_database.Label.value pool)
    registration_disabled_request
    id
  ||> not
  >|> function
  | false -> Lwt.return_false
  | true ->
    Utils.Database.find
      (Pool_database.Label.value pool)
      has_open_sessions_request
      id
;;

let mailing_sending_request =
  let open Caqti_request.Infix in
  {sql|
    SELECT EXISTS(
      SELECT
        1
      FROM
        pool_mailing
      WHERE
        experiment_uuid = UNHEX(REPLACE(?, '-', ''))
        AND pool_mailing.start <= NOW()
        AND pool_mailing.end >= NOW())
    |sql}
  |> Caqti_type.(Repo_entity.Id.t ->! bool)
;;

let mailing_scheduled_request =
  let open Caqti_request.Infix in
  {sql|
    SELECT EXISTS(
      SELECT
        1
      FROM
        pool_mailing
      WHERE
        experiment_uuid = UNHEX(REPLACE(?, '-', ''))
        AND pool_mailing.start >= NOW())
    |sql}
  |> Caqti_type.(Repo_entity.Id.t ->! bool)
;;

let sending_invitations pool id =
  let open Statistics.SendingInvitations in
  let find req = Utils.Database.find (Pool_database.Label.value pool) req id in
  match%lwt find mailing_sending_request with
  | true -> Lwt.return Sending
  | false ->
    (match%lwt find mailing_scheduled_request with
     | true -> Lwt.return Scheduled
     | false -> Lwt.return No)
;;

let session_count_request =
  let open Caqti_request.Infix in
  {sql|
      SELECT
        COUNT(*)
      FROM
        pool_sessions
      WHERE
        experiment_uuid = UNHEX(REPLACE(?, '-', ''))
        AND canceled_at IS NOT NULL
    |sql}
  |> Caqti_type.(Repo_entity.Id.t ->! int)
;;

let session_count pool =
  Utils.Database.find (Pool_database.Label.value pool) session_count_request
;;

let sent_invitation_count_request =
  let open Caqti_request.Infix in
  {sql|
      SELECT
        COUNT(*)
      FROM
        pool_invitations
      WHERE
        experiment_uuid = UNHEX(REPLACE(?, '-', ''))
    |sql}
  |> Caqti_type.(Repo_entity.Id.t ->! int)
;;

let sent_invitation_count pool =
  Utils.Database.find
    (Pool_database.Label.value pool)
    sent_invitation_count_request
;;

let assignment_counts_request =
  let open Caqti_request.Infix in
  {sql|
      SELECT
        COALESCE(SUM(pool_assignments.no_show), 0),
        COALESCE(SUM(pool_assignments.no_show = 0), 0),
        COALESCE(SUM(pool_assignments.participated), 0)
      FROM
        pool_assignments
      INNER JOIN
        pool_sessions ON pool_sessions.uuid = pool_assignments.session_uuid
      WHERE
        pool_sessions.experiment_uuid = UNHEX(REPLACE(?, '-', ''))
        AND pool_sessions.canceled_at IS NULL
        AND pool_assignments.marked_as_deleted = 0
        AND pool_assignments.canceled_at IS NULL
    |sql}
  |> Caqti_type.(Repo_entity.Id.t ->! t3 int int int)
;;

let assignment_counts pool =
  Utils.Database.find (Pool_database.Label.value pool) assignment_counts_request
;;
