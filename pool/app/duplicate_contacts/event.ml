open Entity

type event = Ignored of t [@@deriving eq, show]

let handle_event pool : event -> unit Lwt.t = function
  | Ignored t -> Repo.ignore pool t
[@@deriving eq, show]
;;
