type create =
  { experiment : Experiment_type.public
  ; subject : Subject.t
  }
[@@deriving eq, show]

type event =
  | Created of create
  | Deleted of create
[@@deriving eq, show]

let handle_event pool : event -> unit Lwt.t = function
  | Created { experiment; subject } ->
    Repo_entity.create (Subject.id subject) experiment.Experiment_type.id
    |> Repo.insert pool
  | Deleted { experiment; subject } -> Repo.delete pool subject experiment
;;
