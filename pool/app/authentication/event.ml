open Entity

type event =
  | Created of t
  | Deleted of t
[@@deriving eq, show]

let handle_event pool = function
  | Created t -> Repo.insert pool t
  | Deleted t -> Repo.delete pool t
;;
