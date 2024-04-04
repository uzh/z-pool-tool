open Entity

type update =
  { start_at : StartAt.t
  ; end_at : EndAt.t
  ; limit : Limit.t
  ; distribution : Distribution.t option
  }
[@@deriving eq, show]

type event =
  | Created of (t * Experiment.Id.t)
  | Updated of (update * t)
  | Deleted of t
  | Stopped of t
[@@deriving eq, show, variants]

let handle_event pool =
  let open Utils.Lwt_result.Infix in
  function
  | Created (mailing, experiment_id) ->
    let%lwt () = Repo.insert pool experiment_id mailing in
    Entity_guard.Target.to_authorizable ~ctx:(Database.to_ctx pool) mailing
    ||> Pool_common.Utils.get_or_failwith
    ||> fun (_ : Guard.Target.t) -> ()
  | Updated ({ start_at; end_at; limit; distribution }, mailing) ->
    { mailing with start_at; end_at; limit; distribution } |> Repo.update pool
  | Deleted { id; _ } -> Repo.delete pool id
  | Stopped mailing ->
    { mailing with end_at = Ptime_clock.now () } |> Repo.update pool
;;
