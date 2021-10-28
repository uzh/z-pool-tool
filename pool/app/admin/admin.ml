include Event
include Entity

let user_is_admin pool (user : Sihl_user.t) =
  let%lwt participant =
    Participant.find
      pool
      (user.Sihl.Contract.User.id |> Pool_common.Id.of_string)
  in
  if CCResult.is_error participant
  then (
    let%lwt admin = Repo.find_role_by_user pool user in
    Lwt.return @@ CCResult.is_ok admin)
  else Lwt.return_false
;;

let insert = Repo.insert
let find_by_user = Utils.todo
let find_duplicates = Utils.todo
