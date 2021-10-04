include Event
include Entity

let login _ ~email:_ ~password:_ = Utils.todo ()

let find_root id =
  Repo.find RootC id |> Lwt_result.map_err (fun _ -> "No user found!")
;;

let find_all_root () = Repo.find_all_by_role RootC
let find_by_user = Utils.todo
let find_duplicates = Utils.todo
