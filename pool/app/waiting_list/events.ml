open Entity

type create =
  { experiment : Experiment.Public.t
  ; contact : Contact.t
  }
[@@deriving eq, show]

type update = { comment : Comment.t option } [@@deriving eq, show]

type event =
  | Created of create
  | Updated of update * t
  | Deleted of Entity.t
[@@deriving eq, show]

let handle_event pool : event -> unit Lwt.t = function
  | Created { experiment; contact } ->
    Repo_entity.create (Contact.id contact) experiment.Experiment.Public.id None
    |> Repo.insert pool
  | Updated (command, waiting_list) ->
    { waiting_list with comment = command.comment } |> Repo.update pool
  | Deleted m -> Repo.delete pool m
;;
