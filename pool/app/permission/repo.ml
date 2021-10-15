let insert _ = Utils.todo
let delete _ = Utils.todo

let find_all_by_user _ =
  let result_from_db = [] in
  CCList.map Core.explode_permission result_from_db
  |> CCList.concat
  |> Lwt.return
;;
