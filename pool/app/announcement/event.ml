open Entity

type event =
  | Created of (t * Pool_tenant.Id.t list)
  | Updated of (t * Pool_tenant.Id.t list)
  | Hidden of (t * Pool_common.Id.t)
[@@deriving eq, show]

let handle_event pool =
  let open Utils.Lwt_result.Infix in
  function
  | Created m ->
    let%lwt () = Repo.insert pool m in
    let%lwt () =
      Entity_guard.Target.to_authorizable ~ctx:(Database.to_ctx pool) (fst m)
      ||> Pool_common.Utils.get_or_failwith
      ||> fun (_ : Guard.Target.t) -> ()
    in
    Lwt.return_unit
  | Updated m -> Repo.update pool m
  | Hidden (m, user_id) -> Repo.hide user_id m
;;
