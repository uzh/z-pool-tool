include Event
include Entity

let user_is_admin pool (user : Sihl_user.t) =
  if Sihl_user.is_admin user
  then (
    let%lwt admin = Repo.find_role_by_user pool user in
    Lwt.return @@ CCResult.is_ok admin)
  else Lwt.return_false
;;

let insert = Repo.insert
let find_by_email = Repo.find_by_email
let find_by_user = Utils.todo
let find_duplicates = Utils.todo
