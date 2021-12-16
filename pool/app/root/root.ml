include Event
include Entity

let find_all = Repo.find_all Pool_database.root

let find id =
  let%lwt user =
    Service.User.find_opt
      ~ctx:(Tenant_pool.to_ctx Pool_database.root)
      (id |> Pool_common.Id.value)
  in
  user
  |> CCOption.to_result Pool_common.Message.(NotFound User)
  |> Lwt_result.lift
;;
