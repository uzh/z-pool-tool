include Event
include Entity

let find_all = Repo.find_all Pool_common.Database.root

let find id =
  let%lwt user =
    Service.User.find_opt
      ~ctx:(Pool_common.Utils.pool_to_ctx Pool_common.Database.root)
      (id |> Pool_common.Id.value)
  in
  user
  |> CCOption.to_result Pool_common.Message.(NotFound User)
  |> Lwt_result.lift
;;
