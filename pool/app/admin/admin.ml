include Event
include Entity

let login _ ~email:_ ~password:_ = Utils.todo ()
let find_role_by_user = Repo.find_role_by_user

let user_is_admin user =
  let open Lwt_result.Syntax in
  let* role = find_role_by_user user in
  (match role with
  | `Participant -> false
  | _ -> true)
  |> Lwt.return_ok
;;

let find_by_user = Utils.todo
let find_duplicates = Utils.todo
