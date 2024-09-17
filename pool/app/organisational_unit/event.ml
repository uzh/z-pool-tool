open Entity

type event =
  | Created of t
  | Updated of (t * Name.t)
[@@deriving eq, show]

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

let handle_event ?user_uuid pool : event -> unit Lwt.t =
  let open Utils.Lwt_result.Infix in
  function
  | Created m ->
    let%lwt () = Repo.insert pool m in
    Entity_guard.Target.to_authorizable ~ctx:(Database.to_ctx pool) m
    ||> Pool_common.Utils.get_or_failwith
    ||> fun (_ : Guard.Target.t) -> ()
  | Updated (m, name) ->
    let updated = { m with name } in
    let%lwt () = create_changelog pool ?user_uuid m updated in
    updated |> Repo.update pool
;;
