let src = Logs.Src.create "user_import.service"
let get_or_failwith = Pool_common.Utils.get_or_failwith
let limit = 50

let run database_label =
  let open Utils.Lwt_result.Infix in
  let%lwt import_message =
    let%lwt tenant =
      Pool_tenant.find_by_label database_label ||> get_or_failwith
    in
    Message_template.UserImport.prepare database_label tenant
  in
  let%lwt admins = Repo.find_admins_to_notify database_label limit () in
  let new_limit = limit - CCList.length admins in
  let%lwt contacts =
    if new_limit > 0
    then Repo.find_contacts_to_notify database_label new_limit ()
    else Lwt.return []
  in
  let make_events (messages, events) contact import =
    let message = import_message contact import.Entity.token in
    let event = Event.Notified import in
    message :: messages, event :: events
  in
  let events =
    admins
    |> CCList.fold_left
         (fun acc (admin, import) -> make_events acc (`Admin admin) import)
         ([], [])
  in
  let messages, events =
    contacts
    |> CCList.fold_left
         (fun acc (contact, import) ->
           make_events acc (`Contact contact) import)
         events
  in
  let%lwt () = Email.BulkSent messages |> Email.handle_event database_label in
  events |> Lwt_list.iter_s (Event.handle_event database_label)
;;

let run_all () =
  let open Utils.Lwt_result.Infix in
  (* TODO: Do I need to setup dbs? *)
  Pool_tenant.find_databases ()
  >|> Lwt_list.iter_s (fun { Pool_database.label; _ } -> run label)
;;
