open Entity

type event =
  | AttendanceSet of (t * NoShow.t * Participated.t * ExternalDataId.t option)
  | Canceled of t
  | Created of (t * Session.Id.t)
  | MarkedAsDeleted of t
  | ExternalDataIdUpdated of t * ExternalDataId.t option
  | Updated of t
[@@deriving eq, show, variants]

let handle_event pool : event -> unit Lwt.t = function
  | AttendanceSet (assignment, no_show, participated, external_data_id) ->
    { assignment with
      participated = Some participated
    ; no_show = Some no_show
    ; external_data_id
    }
    |> Repo.update pool
  | Canceled assignment ->
    let%lwt () =
      (* TODO: Check timestamps? Issue #126 *)
      (* TODO: Notification to user? *)
      { assignment with canceled_at = Some (CanceledAt.create_now ()) }
      |> Repo.update pool
    in
    Lwt.return_unit
  | Created (assignment, session_id) ->
    let open Utils.Lwt_result.Infix in
    let%lwt () = Repo.insert pool session_id assignment in
    Entity_guard.Target.to_authorizable
      ~ctx:(Pool_database.to_ctx pool)
      assignment
    ||> Pool_common.Utils.get_or_failwith
    ||> fun (_ : [> `Assignment ] Guard.Target.t) -> ()
  | MarkedAsDeleted assignment -> assignment.id |> Repo.marked_as_deleted pool
  | ExternalDataIdUpdated (assignment, external_data_id) ->
    { assignment with external_data_id } |> Repo.update pool
  | Updated t -> Repo.update pool t
;;
