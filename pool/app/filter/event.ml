open Entity

type event =
  | Created of t
  | Deleted of t
  | Updated of t * t
[@@deriving eq, show, variants]

let handle_event ?user_uuid pool : event -> unit Lwt.t =
  let create_changelog before after =
    let open Version_history in
    insert pool ?user_uuid ~entity_uuid:before.id ~before ~after ()
  in
  function
  | Created t ->
    let open Utils.Lwt_result.Infix in
    let%lwt () = Repo.insert pool t in
    Entity_guard.Target.to_authorizable ~ctx:(Database.to_ctx pool) t
    ||> Pool_common.Utils.get_or_failwith
    ||> fun (_ : Guard.Target.t) -> ()
  | Deleted t -> Repo.delete pool t.id
  | Updated (before, after) ->
    let%lwt () = create_changelog before after in
    Repo.update pool after
;;
