open Entity

type event = Updated of Code.t [@@deriving eq, show]

let handle_event pool : event -> unit Lwt.t = function
  | Updated code -> Repo.insert pool code
;;
