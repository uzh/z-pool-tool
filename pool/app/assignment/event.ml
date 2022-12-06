open Entity

type create =
  { contact : Contact.t
  ; session_id : Pool_common.Id.t
  }
[@@deriving eq, show]

type event =
  | AttendanceSet of (t * ShowUp.t * Participated.t)
  | Canceled of t
  | Created of create
  | Participated of (t * Participated.t)
  | ShowedUp of (t * ShowUp.t)
[@@deriving eq, show]

let handle_event pool : event -> unit Lwt.t = function
  | AttendanceSet (assignment, show_up, participated) ->
    { assignment with participated; show_up } |> Repo.update pool
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
      ~ctx:(Pool_tenant.to_ctx pool)
      assignment
    ||> Pool_common.Utils.get_or_failwith
    ||> fun (_ : [> `Assignment ] Guard.AuthorizableTarget.t) -> ()
  | Participated (assignment, participated) ->
    let%lwt () = { assignment with participated } |> Repo.update pool in
    Lwt.return_unit
  | ShowedUp (assignment, show_up) ->
    let%lwt () = { assignment with show_up } |> Repo.update pool in
    Lwt.return_unit
;;
