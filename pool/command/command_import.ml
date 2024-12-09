let handle_user_import_notifications =
  Command_utils.make_no_args "user_import.notfiy" "Notify users about import" (fun () ->
    let open Utils.Lwt_result.Infix in
    let%lwt () = Database.Pool.initialize () in
    Database.Pool.Tenant.all ()
    |> Lwt_list.iter_s User_import.Service.run
    ||> CCOption.return)
;;
