module Database = Database
module Dynparam = Database.Dynparam
open Caqti_request.Infix
open Entity

module SentInvitations = struct
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

  let find_unique_counts_request =
    {sql|
      SELECT DISTINCT send_count
      FROM pool_invitations
      WHERE experiment_uuid = UNHEX(REPLACE(?, '-', ''))
      ORDER BY send_count
    |sql}
    |> Caqti_type.(string ->* int)
  ;;

  let total_invitation_count_by_experiment pool experiment_id =
    let open Caqti_request.Infix in
    Database.find
      pool
      (count_invitations_request () |> Caqti_type.(string ->! int))
      (Id.value experiment_id)
  ;;

  let by_experiment pool ({ id; _ } as experiment) =
    let open Utils.Lwt_result.Infix in
    let%lwt counts =
      Database.collect pool find_unique_counts_request (Id.value id)
    in
    let base_dyn = Dynparam.(empty |> add Caqti_type.string (Id.value id)) in
    let%lwt total_sent = total_invitation_count_by_experiment pool id in
    let%lwt sent_by_count =
      counts
      |> Lwt_list.map_s (fun send_count ->
        let (Dynparam.Pack (pt, pv)) =
          base_dyn |> Dynparam.add Caqti_type.int send_count
        in
        let request =
          count_invitations_request ~by_count:true () |> pt ->! Caqti_type.int
        in
        Database.find pool request pv
        |> Lwt.map (fun count -> send_count, count))
    in
    let* total_match_filter =
      let query =
        experiment.filter |> CCOption.map (fun { Filter.query; _ } -> query)
      in
      Filter.(
        count_filtered_contacts
          ~include_invited:true
          pool
          (Matcher (Id.to_common id))
          query)
    in
    Lwt.return_ok
      Statistics.SentInvitations.
        { total_sent; total_match_filter; sent_by_count }
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
  |> Caqti_type.(Repo_entity.Id.t ->! bool)
;;

let has_open_sessions_request =
  let open Caqti_request.Infix in
  {sql|
    SELECT
      COUNT(s.uuid) >= 1
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
  Database.find pool registration_disabled_request id
  >|> function
  | true -> Lwt.return_false
  | false -> Database.find pool has_open_sessions_request id
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
  |> Caqti_type.(Repo_entity.Id.t ->! string)
;;

let sending_invitations pool id =
  let open Utils.Lwt_result.Infix in
  let open Statistics.SendingInvitations in
  Database.find pool sending_invitations_request id ||> read
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
  |> Caqti_type.(Repo_entity.Id.t ->! int)
;;

let session_count pool = Database.find pool session_count_request

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
  Database.find pool sent_invitation_count_request
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
  |> Caqti_type.(Repo_entity.Id.t ->! t3 int int int)
;;

let assignment_counts pool = Database.find pool assignment_counts_request
