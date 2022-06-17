open Entity

type update =
  { start_at : StartAt.t
  ; end_at : EndAt.t
  ; rate : Rate.t
  ; distribution : Distribution.t option
  }
[@@deriving eq, show]

type event =
  | Created of (t * Pool_common.Id.t)
  | Updated of (update * t)
  | Deleted of t
  | Stopped of t
[@@deriving eq, show]

let handle_event pool = function
  | Created (mailing, experiment_id) -> Repo.insert pool experiment_id mailing
  | Updated ({ start_at; end_at; rate; distribution }, mailing) ->
    { mailing with start_at; end_at; rate; distribution } |> Repo.update pool
  | Deleted { id; _ } -> Repo.delete pool id
  | Stopped mailing ->
    { mailing with end_at = Ptime_clock.now () } |> Repo.update pool
;;
