let with_transaction _ _ = failwith "todo"

let find request input =
  Sihl.Database.query (fun connection ->
      let module Connection = (val connection : Caqti_lwt.CONNECTION) in
      Connection.find request input
      |> Lwt_result.map_err (fun err -> Caqti_error.show err))
;;

let find_opt request input =
  Sihl.Database.query (fun connection ->
      let module Connection = (val connection : Caqti_lwt.CONNECTION) in
      Connection.find_opt request input
      |> Lwt_result.map_err (fun err -> Caqti_error.show err))
;;

let collect request input =
  Sihl.Database.query (fun connection ->
      let module Connection = (val connection : Caqti_lwt.CONNECTION) in
      Connection.collect_list request input
      |> Lwt_result.map_err (fun err -> Caqti_error.show err))
;;

let exec request input =
  Sihl.Database.query (fun connection ->
      let module Connection = (val connection : Caqti_lwt.CONNECTION) in
      Connection.exec request input
      |> Lwt_result.map_err (fun err -> Caqti_error.show err))
;;
