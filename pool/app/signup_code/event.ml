open Entity

type event =
  | SignedUp of Code.t
  | Verified of Code.t
[@@deriving eq, show, variants]

let handle_event pool : event -> unit Lwt.t = function
  | SignedUp code -> Repo.insert pool `Signup code
  | Verified code -> Repo.insert pool `Verification code
;;
