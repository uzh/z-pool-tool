module Database = Pool_database
open Caqti_request.Infix
open Entity.Experiment
module RepoId = Experiment.Repo.Entity.Id

module SentInvitations = struct
  let find_unique_counts_request =
    {sql|
      SELECT DISTINCT send_count
      FROM pool_invitations
      WHERE experiment_uuid = UNHEX(REPLACE(?, '-', ''))
      ORDER BY send_count
    |sql}
    |> Caqti_type.(string ->* int)
  ;;

  let by_experiment pool ({ Experiment.id; _ } as experiment) =
    let open Utils.Lwt_result.Infix in
    let%lwt counts =
      Utils.Database.collect
        (pool |> Database.Label.value)
        find_unique_counts_request
        (Experiment.Id.value id)
    in
    let%lwt total_sent = Experiment.invitation_count pool id in
    let%lwt sent_by_count =
      counts
      |> Lwt_list.map_s (fun send_count ->
        Experiment.invitation_count_by_iteration pool id send_count
        |> Lwt.map (fun count -> send_count, count))
    in
    let* total_match_filter =
      let query =
        experiment.Experiment.filter
        |> CCOption.map (fun { Filter.query; _ } -> query)
      in
      Filter.(
        count_filtered_contacts
          ~include_invited:true
          pool
          (Matcher (Experiment.Id.to_common id))
          query)
    in
    Lwt.return_ok
      SentInvitations.{ total_sent; total_match_filter; sent_by_count }
  ;;
end

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
  |> Caqti_type.(RepoId.t ->! bool)
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
  |> Caqti_type.(RepoId.t ->! bool)
;;

let registration_possible pool id =
  let open Utils.Lwt_result.Infix in
  Utils.Database.find
    (Database.Label.value pool)
    registration_disabled_request
    id
  ||> not
  >|> function
  | false -> Lwt.return_false
  | true ->
    Utils.Database.find (Database.Label.value pool) has_open_sessions_request id
;;

let sending_invitations_request =
  let open Caqti_request.Infix in
  {sql|
    SELECT COALESCE(
    (SELECT
      DISTINCT (CASE
          WHEN CURRENT_TIMESTAMP BETWEEN start AND end THEN 'sending'
          WHEN start > CURRENT_TIMESTAMP THEN 'scheduled'
          ELSE 'no'
      END) AS status
    FROM
      pool_mailing
    WHERE
      experiment_uuid = UNHEX(REPLACE(?, '-', ''))
      ORDER BY FIELD(status, 'sending', 'scheduled', 'no')
      LIMIT 1),
    'no')
  |sql}
  |> Caqti_type.(RepoId.t ->! string)
;;

let sending_invitations pool id =
  let open Utils.Lwt_result.Infix in
  let open Entity.Experiment.SendingInvitations in
  Utils.Database.find (Database.Label.value pool) sending_invitations_request id
  ||> read
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
        AND canceled_at IS NULL
    |sql}
  |> Caqti_type.(RepoId.t ->! int)
;;

let session_count pool =
  Utils.Database.find (Database.Label.value pool) session_count_request
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
  |> Caqti_type.(RepoId.t ->! int)
;;

let sent_invitation_count pool =
  Utils.Database.find (Database.Label.value pool) sent_invitation_count_request
;;

let assignment_counts_request =
  let open Caqti_request.Infix in
  {sql|
      SELECT
        COALESCE(SUM(pool_assignments.no_show = 0), 0),
        COALESCE(SUM(pool_assignments.no_show), 0),
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
  |> Caqti_type.(RepoId.t ->! t3 int int int)
;;

let assignment_counts pool =
  Utils.Database.find (Database.Label.value pool) assignment_counts_request
;;
