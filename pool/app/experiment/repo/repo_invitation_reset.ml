open Entity.InvitationReset
open Caqti_request.Infix
open Utils.Lwt_result.Infix

let t =
  let encode _ = Pool_common.Utils.failwith Pool_message.Error.ReadOnlyModel in
  let decode (created_at, iteration, contacts_matching_filter, invitations_sent) =
    let open CCResult in
    (* TODO: Could be solved using a join instead *)
    let created_at =
      Utils.Ptime.to_local_date created_at |> Pool_common.CreatedAt.create
    in
    Ok { created_at; iteration; contacts_matching_filter; invitations_sent }
  in
  Caqti_type.(custom ~encode ~decode (t4 ptime int int int))
;;

let write =
  let open Write in
  let decode _ = Pool_common.Utils.failwith Pool_message.Error.WriteOnlyModel in
  let encode ({ experiment_id; contacts_matching_filter; invitations_sent } : t) =
    Ok (experiment_id, contacts_matching_filter, invitations_sent)
  in
  Caqti_type.(custom ~encode ~decode (t3 Repo_entity.Id.t int int))
;;

let sql_select_columns =
  [ "pool_experiment_invitation_reset.created_at"
  ; "ROW_NUMBER() OVER (ORDER BY pool_experiment_invitation_reset.created_at ASC)"
  ; "pool_experiment_invitation_reset.contacts_matching_filter"
  ; "pool_experiment_invitation_reset.sent_invitations"
  ]
;;

let select_sql where =
  Format.sprintf
    "SELECT %s FROM pool_experiment_invitation_reset %s"
    (CCString.concat ", " sql_select_columns)
    where
;;

let find_by_experiment_sql =
  {sql|
    WHERE experiment_uuid = UNHEX(REPLACE(?, '-', ''))
  |sql}
  |> select_sql
;;

let find_by_experiment_request =
  Format.asprintf "%s ORDER BY created_at ASC" find_by_experiment_sql
  |> Repo_entity.Id.t ->* t
;;

let find_by_experiment_opt_request =
  find_by_experiment_sql
  |> Format.asprintf "%s ORDER BY created_at DESC LIMIT 1"
  |> Repo_entity.Id.t ->? t
;;

let find_by_experiment pool = Database.collect pool find_by_experiment_request
let find_latest_by_experiment pool = Database.find_opt pool find_by_experiment_opt_request

let insert_request =
  {sql|
    INSERT INTO pool_experiment_invitation_reset (
      experiment_uuid, 
      contacts_matching_filter, 
      sent_invitations
    ) VALUES (
      UNHEX(REPLACE(?, '-', '')), 
      ?, 
      ?
    )
  |sql}
  |> Caqti_type.(t3 Repo_entity.Id.t int int ->. unit)
;;

let sent_invitations_request =
  {sql|
    SELECT COUNT(queue_uuid)
    FROM
      pool_queue_job_invitation
      INNER JOIN pool_invitations ON pool_queue_job_invitation.invitation_uuid = pool_invitations.uuid
      INNER JOIN ((SELECT uuid, clone_of FROM pool_queue_jobs) UNION (SELECT uuid, clone_of FROM pool_queue_jobs_history)) as queue ON pool_queue_job_invitation.queue_uuid = queue.uuid
      WHERE
        pool_invitations.experiment_uuid = UNHEX(REPLACE($1, '-', ''))
        AND queue.clone_of IS NULL
        AND pool_queue_job_invitation.created_at > COALESCE(
    (
      SELECT created_at
      FROM pool_experiment_invitation_reset
      WHERE experiment_uuid = UNHEX(REPLACE($1, '-', ''))
      ORDER BY created_at DESC
      LIMIT 1
    ),
    '1970-01-01 00:00:00'
  )
  |sql}
  |> Caqti_type.(Repo_entity.Id.t ->! int)
;;

let invitations_sent_since_last_reset pool = Database.find pool sent_invitations_request

let insert pool { Entity.id; filter; _ } =
  let%lwt sent_invitations = invitations_sent_since_last_reset pool id in
  let%lwt contacts_matching_filter =
    let open Filter in
    filter
    |> CCOption.map (fun { query; _ } -> query)
    |> count_filtered_contacts
         ~include_invited:true
         pool
         (Matcher (Entity.Id.to_common id))
    ||> Pool_common.Utils.get_or_failwith
  in
  Database.exec pool insert_request (id, contacts_matching_filter, sent_invitations)
;;
