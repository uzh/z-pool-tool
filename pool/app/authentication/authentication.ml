include Entity
include Event

let create ~user ~channel =
  { id = Id.create (); user_uuid = Pool_user.id user; channel; token = Token.generate () }
;;

let find_valid_by_id = Repo.find_valid_by_id
