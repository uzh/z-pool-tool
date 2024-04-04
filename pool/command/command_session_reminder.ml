let get_or_failwith = Pool_common.Utils.get_or_failwith

let tenant_specific_session_reminder =
  Command_utils.make_pool_specific
    "session_reminder.send"
    "Send session reminders of specified tenant"
    (fun pool ->
       let open Utils.Lwt_result.Infix in
       pool
       |> Pool_tenant.find_by_label
       ||> get_or_failwith
       >|> Reminder.Service.send_tenant_reminder
       ||> CCOption.some)
;;

let all_tenants_session_reminder =
  Command_utils.make_no_args
    "session_reminder.send_all"
    "Send session reminders of all tenants"
    (fun () ->
       let open Utils.Lwt_result.Infix in
       let%lwt (_ : Database.Label.t list) = Command_utils.setup_databases () in
       Reminder.Service.run () ||> CCOption.some)
;;
