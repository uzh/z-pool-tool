open Entity

type create =
  { contact : Contact.t
  ; session_id : Session.Id.t
  }
[@@deriving eq, show]

type event =
  | AttendanceSet of (t * NoShow.t * Participated.t * ExternalDataId.t option)
  | Canceled of t
  | Created of create
  | MarkedAsDeleted of t
  | ExternalDataIdUpdated of t * ExternalDataId.t option
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
  | Created { contact; session_id } ->
    let open Utils.Lwt_result.Infix in
    let assignment = create contact in
    let%lwt () = Repo.insert pool session_id assignment in
    Entity_guard.Target.to_authorizable
      ~ctx:(Pool_database.to_ctx pool)
      assignment
    ||> Pool_common.Utils.get_or_failwith
    ||> fun (_ : [> `Assignment ] Guard.Target.t) -> ()
  | MarkedAsDeleted assignment -> assignment.id |> Repo.marked_as_deleted pool
  | ExternalDataIdUpdated (assignment, external_data_id) ->
    { assignment with external_data_id } |> Repo.update pool
;;
