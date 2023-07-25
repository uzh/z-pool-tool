open Utils.Lwt_result.Infix
open Entity

type event =
  | Created of t
  | Updated of t
  | Tagged of Tagged.t
  | Untagged of Tagged.t
  | ParticipationTagAssigned of Pool_common.Id.t * Id.t
  | ParticipationTagRemoved of Pool_common.Id.t * Id.t
[@@deriving eq, show, variants]

let handle_event pool : event -> unit Lwt.t =
  let ctx = Pool_database.to_ctx pool in
  function
  | Created tag ->
    let%lwt (_ : (unit, Pool_common.Message.error) result) =
      Repo.insert pool tag >|- Pool_common.Utils.with_log_error
    in
    Entity_guard.Target.to_authorizable ~ctx tag
    ||> Pool_common.Utils.get_or_failwith
    ||> fun (_ : Role.Target.t Guard.Target.t) -> ()
  | Updated tag ->
    let%lwt () = Repo.update pool tag in
    Lwt.return_unit
  | Tagged tagged ->
    let%lwt (_ : (unit, Pool_common.Message.error) result) =
      Repo.insert_tagged pool tagged >|- Pool_common.Utils.with_log_error
    in
    Lwt.return_unit
  | Untagged tagged ->
    let%lwt () = Repo.delete_tagged pool tagged in
    Lwt.return_unit
  | ParticipationTagAssigned (id, tag_id) ->
    Repo_participation_tags.insert pool (id, tag_id)
  | ParticipationTagRemoved (id, tag_id) ->
    Repo_participation_tags.delete pool (id, tag_id)
;;
