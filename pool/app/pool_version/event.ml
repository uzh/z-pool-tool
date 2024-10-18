open Entity

type event =
  | Created of t
  | Updated of t
  | Published of t
[@@deriving eq, show]

let handle_event pool =
  let open Utils.Lwt_result.Infix in
  function
  | Created m ->
    let%lwt () = Repo.insert pool m in
    let%lwt () =
      Entity_guard.Target.to_authorizable ~ctx:(Database.to_ctx pool) m
      ||> Pool_common.Utils.get_or_failwith
      ||> fun (_ : Guard.Target.t) -> ()
    in
    Lwt.return_unit
  | Updated m -> Repo.update pool m
  | Published m ->
    Repo.update pool { m with published_at = Some (PublishedAt.create_now ()) }
;;
