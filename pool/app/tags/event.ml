open Utils.Lwt_result.Infix
open Entity

type event =
  | Created of t
  | Updated of (t * t)
  | Tagged of Tagged.t
  | Untagged of Tagged.t
  | ParticipationTagAssigned of Repo_participation_tags.entity * Id.t
  | ParticipationTagRemoved of Repo_participation_tags.entity * Id.t
[@@deriving eq, show, variants]

let handle_event ?user_uuid pool : event -> unit Lwt.t =
  let ctx = Database.to_ctx pool in
  let create_changelog before after =
    let open Version_history in
    insert pool ?user_uuid ~entity_uuid:before.id ~before ~after ()
  in
  function
  | Created tag ->
    let%lwt (_ : (unit, Pool_message.Error.t) result) =
      Repo.insert pool tag >|- Pool_common.Utils.with_log_error
    in
    Entity_guard.Target.to_authorizable ~ctx tag
    ||> Pool_common.Utils.get_or_failwith
    ||> fun (_ : Guard.Target.t) -> ()
  | Updated (before, after) ->
    let%lwt () = create_changelog before after in
    let%lwt () = Repo.update pool after in
    Lwt.return_unit
  | Tagged tagged ->
    let%lwt (_ : (unit, Pool_message.Error.t) result) =
      Repo.insert_tagged pool tagged >|- Pool_common.Utils.with_log_error
    in
    Lwt.return_unit
  | Untagged tagged ->
    let%lwt () = Repo.delete_tagged pool tagged in
    Lwt.return_unit
  | ParticipationTagAssigned (entity, tag_id) ->
    Repo_participation_tags.(insert pool (get_id entity, tag_id))
  | ParticipationTagRemoved (entity, tag_id) ->
    Repo_participation_tags.(delete pool (get_id entity, tag_id))
;;
