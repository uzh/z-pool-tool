open Entity

type event =
  | Created of t
  | Deleted of t
  | ResetExpired
[@@deriving eq, show]

let handle_event pool = function
  | Created t -> Repo.insert pool t
  | Deleted t -> Repo.delete pool t
  | ResetExpired -> Repo.reset_expired pool ()
;;
