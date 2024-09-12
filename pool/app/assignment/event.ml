open Entity

type event =
  | Canceled of t
  | Created of (t * Session.Id.t)
  | MarkedAsDeleted of t
  | ExternalDataIdUpdated of t * ExternalDataId.t option
  | Updated of t
[@@deriving eq, show, variants]

let handle_event pool : event -> unit Lwt.t = function
  | Canceled assignment ->
    let%lwt () = Repo.update pool assignment in
    Lwt.return_unit
  | Created (assignment, session_id) ->
    let open Utils.Lwt_result.Infix in
    let%lwt () = Repo.insert pool session_id assignment in
    Entity_guard.Target.to_authorizable ~ctx:(Database.to_ctx pool) assignment
    ||> Pool_common.Utils.get_or_failwith
    ||> fun (_ : Guard.Target.t) -> ()
  | MarkedAsDeleted assignment -> assignment.id |> Repo.marked_as_deleted pool
  | ExternalDataIdUpdated (assignment, external_data_id) ->
    { assignment with external_data_id } |> Repo.update pool
  | Updated t -> Repo.update pool t
;;
