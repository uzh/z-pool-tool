let handle_user_import_notifications =
  Command_utils.make_no_args
    "user_import.notfiy"
    "Notify users about import"
    (fun () ->
    let open Utils.Lwt_result.Infix in
    Command_utils.setup_databases ()
    >|> Lwt_list.iter_s User_import.Service.run
    ||> CCOption.return)
;;
