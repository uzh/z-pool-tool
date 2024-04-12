open Entity

type create =
  { start : Session.Start.t
  ; end_at : Session.End.t
  ; internal_description : Session.InternalDescription.t option
  ; public_description : Session.PublicDescription.t option
  ; max_participants : Session.ParticipantAmount.t option
  }
[@@deriving eq, show]

type event = Created of t [@@deriving eq, show]

let handle_event pool =
  let open Utils.Lwt_result.Infix in
  function
  | Created time_window ->
    let%lwt () = Repo.insert pool time_window in
    Entity_guard.Target.to_authorizable
      ~ctx:(Pool_database.to_ctx pool)
      time_window
    ||> Pool_common.Utils.get_or_failwith
    ||> fun (_ : Guard.Target.t) -> ()
;;
