module Database = Database
module Dynparam = Database.Dynparam
open Caqti_request.Infix
open Entity

let assignment_counts =
  let encode _ = Pool_common.Utils.failwith Pool_message.Error.ReadOnlyModel in
  let decode (show_up_count, no_show_count, participation_count) =
    Ok { show_up_count; no_show_count; participation_count }
  in
  Caqti_type.(custom ~encode ~decode (t3 int int int))
;;

module FilterStatistics = struct
  let count_invited_contacts_request =
    {sql|
      SELECT COUNT(1)
      FROM pool_invitations
        INNER JOIN pool_contacts ON pool_contacts.user_uuid = pool_invitations.contact_uuid
        INNER JOIN user_users ON user_users.uuid = pool_contacts.user_uuid
      WHERE 
        experiment_uuid = UNHEX(REPLACE(?, '-', ''))
        AND pool_contacts.paused != 1
        AND pool_contacts.disabled = 0
        AND user_users.status = "active"
    |sql}
    |> Caqti_type.(string ->! int)
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

  let invited_contacts_count pool experiment_id =
    Database.find pool count_invited_contacts_request (Id.value experiment_id)
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
  Database.find pool sending_invitations_request id ||> SendingInvitations.read
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

let sent_invitation_count pool = Database.find pool sent_invitation_count_request

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
  |> Repo_entity.Id.t ->! assignment_counts
;;

let assignment_counts pool = Database.find pool assignment_counts_request
