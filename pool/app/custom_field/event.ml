open Entity

type event =
  | Created of t
  | Updated of t
[@@deriving eq, show, variants]

let handle_event pool : event -> unit Lwt.t = function
  | Created t -> Repo.insert pool t
  | Updated t -> Repo.update pool t
;;
