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
[@@deriving eq, show]

let handle_event pool : event -> unit Lwt.t = function
  | AttendanceSet (assignment, show_up, participated) ->
    { assignment with participated; show_up } |> Repo.update pool
  | Canceled assignment ->
    let%lwt () =
      (* TODO: Check timestamp? Is it UTC? Use DB NOW()? *)
      { assignment with canceled_at = Some (CanceledAt.create_now ()) }
      |> Repo.update pool
    in
    Lwt.return_unit
  | Created { contact; session_id } ->
    contact |> create |> Repo.insert pool session_id
;;
