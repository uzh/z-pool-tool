open Entity

type event =
  | Created of t
  | Updated of (t * Name.t * Pool_common.Id.t)
[@@deriving eq, show]

let handle_event pool : event -> unit Lwt.t =
  let open Utils.Lwt_result.Infix in
  function
  | Created m ->
    let%lwt () = Repo.insert pool m in
    Entity_guard.Target.to_authorizable ~ctx:(Database.to_ctx pool) m
    ||> Pool_common.Utils.get_or_failwith
    ||> fun (_ : Guard.Target.t) -> ()
  | Updated (m, name, user_uuid) ->
    let updated = { m with name } in
    let%lwt () =
      Version_history.create
        pool
        ~entity_uuid:(Id.to_common m.id)
        ~user_uuid
        ~before:m
        ~after:updated
        ()
    in
    updated |> Repo.update pool
;;
