include Event
include Entity

let user_is_admin pool (user : Sihl_user.t) =
  let%lwt participant =
    Participant.find
      pool
      (user.Sihl.Contract.User.id |> Pool_common.Id.of_string)
  in
  match participant with
  | Ok _ -> Lwt.return false
  | Error _ ->
    let%lwt admin = Repo.find_role_by_user pool user in
    (match admin with
    | Error _ -> Lwt.return false
    | Ok _ -> Lwt.return true)
;;

let insert = Repo.insert
let find_by_user = Utils.todo
let find_duplicates = Utils.todo
