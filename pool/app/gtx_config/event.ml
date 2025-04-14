open Entity

type event =
  | CacheCleared
  | Created of t
  | Updated of t * t
  | Removed
[@@deriving eq, show]

let handle_event ?user_uuid pool =
  let create_changelog before after =
    let open Version_history in
    insert pool ?user_uuid ~entity_uuid:before.id ~before ~after ()
  in
  function
  | CacheCleared -> Repo.Cache.clear () |> Lwt.return
  | Created t -> Repo.insert pool t
  | Updated (before, after) ->
    let%lwt () = Repo.update pool after in
    create_changelog before after
  | Removed -> Repo.destroy pool ()
;;
