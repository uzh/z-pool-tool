type create =
  { experiment : Experiment.Public.t
  ; contact : Contact.t
  }
[@@deriving eq, show]

type event =
  | Created of create
  | Deleted of create
[@@deriving eq, show]

let handle_event pool : event -> unit Lwt.t = function
  | Created { experiment; contact } ->
    Repo_entity.create (Contact.id contact) experiment.Experiment.Public.id
    |> Repo.insert pool
  | Deleted { experiment; contact } -> Repo.delete pool contact experiment
;;
