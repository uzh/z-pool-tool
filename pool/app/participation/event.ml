open Entity

type create =
  { contact : Contact.t
  ; session_id : Pool_common.Id.t
  }
[@@deriving eq, show]

type event =
  | Canceled of t
  | Created of create
  | Participated of t * Participated.t
  | ShowedUp of t * ShowUp.t
[@@deriving eq, show]

let handle_event pool : event -> unit Lwt.t = function
  | Canceled participation ->
    let%lwt () =
      { participation with canceled_at = CanceledAt.create_now () }
      |> Repo.update pool
    in
    Lwt.return_unit
  | Created { contact; session_id } ->
    contact |> create |> Repo.insert pool session_id
  | Participated (participation, participated) ->
    let%lwt () = { participation with participated } |> Repo.update pool in
    Lwt.return_unit
  | ShowedUp (participation, show_up) ->
    let%lwt () = { participation with show_up } |> Repo.update pool in
    Lwt.return_unit
;;
