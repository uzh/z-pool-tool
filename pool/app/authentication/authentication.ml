include Entity
include Event

let create ?(id = Id.create ()) ?(token = Token.generate ()) ~user ~channel () =
  { id; user_uuid = Pool_user.id user; channel; token }
;;

let find_valid_by_id = Repo.find_valid_by_id
