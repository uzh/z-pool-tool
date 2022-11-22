include Event
include Entity
module Guard = Entity_guard
module Repo = Repo

let find = Repo.find
let find_all = Repo.find_all

let user_is_admin pool (user : Sihl_user.t) =
  if Sihl_user.is_admin user
  then (
    let%lwt admin = find pool (Pool_common.Id.of_string user.Sihl_user.id) in
    Lwt.return @@ CCResult.is_ok admin)
  else Lwt.return_false
;;
