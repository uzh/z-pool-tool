let insert _ _ = Sihl.todo
let delete _ _ = Sihl.todo

let find_all_by_user _ =
  let result_from_db = [] in
  List.map Core.explode_permission result_from_db |> List.concat |> Lwt.return
;;
