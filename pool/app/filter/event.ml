open Entity

type create =
  { id : Pool_common.Id.t
  ; filter : filter
  }
[@@deriving eq, show]

type event = Created of t [@@deriving eq, show]

let handle_event pool : event -> unit Lwt.t = function
  | Created t -> Repo.insert pool t
;;
