open Entity
module Reminder = Pool_common.Reminder

type event =
  | Created of t
  | Updated of t * t
  (* TODO: Should I move this into the event, and throw an exception, if it fails? *)
  | ResetInvitations of InvitationReset.Write.t
  | Deleted of Pool_common.Id.t
[@@deriving eq, show, variants]

let handle_event ?user_uuid pool : event -> unit Lwt.t =
  let open Utils.Lwt_result.Infix in
  let ctx = Database.to_ctx pool in
  let create_changelog before after =
    let open Version_history in
    insert pool ?user_uuid ~entity_uuid:before.id ~before ~after ()
  in
  function
  | Created t ->
    let%lwt () = Repo.insert pool t in
    Entity_guard.Target.to_authorizable ~ctx t
    ||> Pool_common.Utils.get_or_failwith
    ||> fun (_ : Guard.Target.t) -> ()
  | Updated (experiment, updated) ->
    let%lwt () = create_changelog experiment updated in
    Repo.update pool updated
  | ResetInvitations reset -> Repo_invitation_reset.insert pool reset
  | Deleted experiment_id -> Repo.delete pool experiment_id
[@@deriving eq, show]
;;
