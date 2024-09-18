open Entity
module Reminder = Pool_common.Reminder

let create_changelog pool ?user_uuid before after =
  let open Version_history in
  user_uuid
  |> CCOption.map_or ~default:Lwt.return_unit (fun user_uuid ->
    insert
      pool
      ~entity_uuid:(Entity.Id.to_common before.id)
      ~user_uuid
      ~before
      ~after
      ())
;;

type event =
  | Created of t
  | Updated of t * t
  | ResetInvitations of t
  | Deleted of Pool_common.Id.t
[@@deriving eq, show, variants]

let handle_event ?user_uuid pool : event -> unit Lwt.t =
  let open Utils.Lwt_result.Infix in
  let ctx = Database.to_ctx pool in
  function
  | Created t ->
    let%lwt () = Repo.insert pool t in
    Entity_guard.Target.to_authorizable ~ctx t
    ||> Pool_common.Utils.get_or_failwith
    ||> fun (_ : Guard.Target.t) -> ()
  | Updated (experiment, updated) ->
    let%lwt () = create_changelog pool ?user_uuid experiment updated in
    Repo.update pool updated
  | ResetInvitations t ->
    let%lwt experiment =
      Repo.find pool t.id ||> Pool_common.Utils.get_or_failwith
    in
    Repo.update
      pool
      { experiment with
        invitation_reset_at = Some (InvitationResetAt.create_now ())
      }
  | Deleted experiment_id -> Repo.delete pool experiment_id
[@@deriving eq, show]
;;
