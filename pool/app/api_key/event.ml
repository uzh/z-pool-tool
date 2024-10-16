open Entity

type event =
  | Created of t
  | Updated of (t * t)
  | Destroyed of t
[@@deriving eq, show]

let handle_event ?tags pool =
  let open Utils.Lwt_result.Infix in
  function
  | Created m ->
    let roles = [] in
    let ctx = Database.to_ctx pool in
    let%lwt () = Repo.insert pool m in
    let%lwt () =
      Entity_guard.Target.to_authorizable ~ctx m
      ||> Pool_common.Utils.get_or_failwith
      ||> fun (_ : Guard.Target.t) -> ()
    in
    let%lwt () =
      let open Guard in
      let open Pool_common.Utils in
      let%lwt authorizable =
        Entity_guard.Actor.to_authorizable ~ctx m
        >|- with_log_error ~src ?tags
        ||> get_or_failwith
      in
      Lwt_list.iter_s
        (fun (role, target_uuid) ->
          ActorRole.create ?target_uuid authorizable.Actor.uuid role
          |> Persistence.ActorRole.upsert ~ctx)
        roles
    in
    Lwt.return_unit
  | Updated (_, after) -> Repo.update pool after
  | Destroyed m -> Repo.destroy pool m
;;
