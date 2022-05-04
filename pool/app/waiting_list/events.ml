open Entity

type create =
  { experiment : Experiment.t
  ; subject : Subject.t
  }
[@@deriving eq, show]

type event = Created of create [@@deriving eq, show]

let handle_event pool : event -> unit Lwt.t = function
  | Created { experiment; subject } ->
    create subject experiment |> Repo.insert pool
;;
