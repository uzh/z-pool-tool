open Entity

type create =
  { experiment_id : Pool_common.Id.t
  ; subject : Subject.t
  }
[@@deriving eq, show]

type event =
  | Created of create
  | Resent of t
[@@deriving eq, show]

let handle_event pool : event -> unit Lwt.t = function
  | Created { experiment_id; subject } ->
    let%lwt () = create subject |> Repo.insert pool experiment_id in
    (* TODO send invitation email *)
    Lwt.return_unit
  | Resent _ ->
    (* TODO send invitation email *)
    Lwt.return_unit
;;
