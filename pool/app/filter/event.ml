open Entity

type event =
  | Created of t
  | Deleted of t
  | Updated of t
[@@deriving eq, show, variants]

let handle_event pool : event -> unit Lwt.t = function
  | Created t ->
    let open Utils.Lwt_result.Infix in
    let%lwt () = Repo.insert pool t in
    Entity_guard.Target.to_authorizable ~ctx:(Pool_tenant.to_ctx pool) t
    ||> Pool_common.Utils.get_or_failwith
    ||> fun (_ : [> `Filter ] Guard.Target.t) -> ()
  | Deleted t -> Repo.delete pool t.id
  | Updated t -> Repo.update pool t
;;
