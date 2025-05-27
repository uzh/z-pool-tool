open Entity

type event =
  | Created of t
  | Updated of (t * Name.t)
[@@deriving eq, show]

let handle_event ?user_uuid pool : event -> unit Lwt.t =
  let open Utils.Lwt_result.Infix in
  let create_changelog before after =
    let open Version_history in
    insert pool ?user_uuid ~entity_uuid:before.id ~before ~after ()
  in
  function
  | Created m ->
    let%lwt () = Repo.insert pool m in
    Entity_guard.Target.to_authorizable ~ctx:(Database.to_ctx pool) m
    ||> Pool_common.Utils.get_or_failwith
    ||> fun (_ : Guard.Target.t) -> ()
  | Updated (m, name) ->
    let updated = { m with name } in
    let%lwt () = create_changelog m updated in
    updated |> Repo.update pool
;;
