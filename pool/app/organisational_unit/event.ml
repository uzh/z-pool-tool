open Entity

type event =
  | Created of t
  | Updated of (t * Name.t)
[@@deriving eq, show]

let handle_event pool : event -> unit Lwt.t =
  let open Utils.Lwt_result.Infix in
  function
  | Created m ->
    let%lwt () = Repo.insert pool m in
    Entity_guard.Target.to_authorizable ~ctx:(Database.to_ctx pool) m
    ||> Pool_common.Utils.get_or_failwith
    ||> fun (_ : Guard.Target.t) -> ()
  | Updated (m, name) -> { m with name } |> Repo.update pool
;;
