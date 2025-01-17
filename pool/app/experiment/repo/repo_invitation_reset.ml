open Entity.InvitationReset

let t =
  let encode _ = Pool_common.Utils.failwith Pool_message.Error.ReadOnlyModel in
  let decode (created_at, iteration, contacts_matching_filter, invitations_sent) =
    let open CCResult in
    Ok { created_at; iteration; contacts_matching_filter; invitations_sent }
  in
  Caqti_type.(custom ~encode ~decode (t4 Pool_common.Repo.CreatedAt.t int int int))
;;

let sql_select_columns =
  [ "pool_experiment_invitation_reset.created_at"
  ; "pool_experiment_invitation_reset.contacts_matching_filter"
  ; "pool_experiment_invitation_reset.sent_invitations"
  ]
;;

(* TODO: The sent_invitations column could theoretically be calculated instead of stored *)
(* let find_current pool experiment_id =
  let open Caqti_request.Infix in
  let current_request =
    {sql|
      SELECT
        pool_experiment_invitation_reset.created_at,
        ROW_NUMBER() OVER (PARTITION BY experiment_uuid ORDER BY created_at) AS iteration,
        pool_experiment_invitation_reset.contacts_matching_filter,
        pool_experiment_invitation_reset.sent_invitations
      FROM pool_experiment_invitation_reset
      WHERE experiment_uuid = UNHEX(REPLACE(?, '-', ''))
      ORDER BY created_at DESC
      LIMIT 1
    |sql}
    |> Repo_entity.Id.t ->! t
  in
  let default () =
    let%lwt invitations_sent =
      Repo_statistics.SentInvitations.total_invitation_count_by_experiment
        pool
        experiment_id
    in
    ()
  in
  Database.find_opt pool current_request experiment_id
;; *)
