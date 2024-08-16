open Database
open Utils.Lwt_result.Infix

let root =
  Command_utils.make_no_args "migrate.root" "Migrate root database" (fun () ->
    let (_ : status) = Root.add () in
    let%lwt () =
      Migration.execute root (Pool_database.Root.steps ())
      ||> function
      | Ok () -> exit 0
      | Error _ -> exit 1
    in
    Lwt.return_some ())
;;

let notify_about_failure db_label error =
  let send () =
    let open Sihl_email in
    let read_config = Sihl.Configuration.read_string in
    let sender = read_config "SMTP_SENDER" in
    let recipient = read_config "SMTP_SENDER" in
    match sender, recipient with
    | Some sender, Some recipient ->
      let text =
        Format.asprintf
          {|Migrations on the tenant %s could not be executed successfully.
          
Error: %s

Please take the necessary actions.|}
          (Database.Label.value db_label)
          error
      in
      let email =
        { sender
        ; recipient
        ; subject = "Tenant migration failed"
        ; text
        ; html = None
        ; cc = []
        ; bcc = []
        }
      in
      let%lwt { Email.Service.Smtp.sender
              ; reply_to
              ; recipients
              ; subject
              ; body
              ; config
              }
        =
        Email.Service.Smtp.prepare Database.root email
      in
      Letters.create_email ~reply_to ~from:sender ~recipients ~subject ~body ()
      |> (function
       | Ok message -> Letters.send ~config ~sender ~recipients ~message
       | Error msg -> raise (Sihl.Contract.Email.Exception msg))
    | _, _ -> Lwt.return_unit
  in
  Lwt.catch
    (fun () ->
      if Sihl.Configuration.is_development ()
      then Logs.err (fun m -> m "%s" error) |> Lwt.return
      else send ())
    (fun exn ->
      let err = Printexc.to_string exn in
      let error_message =
        Format.asprintf "Error while notifying about failed migration '%s'" err
      in
      Logs.err (fun m -> m "%s" error_message) |> Lwt.return)
;;

let migrate_tenants db_labels =
  let run db_label =
    let set_status = Database.Tenant.update_status db_label in
    let handle_error err =
      let err = Pool_common.(Utils.error_to_string Language.En err) in
      let%lwt () = set_status Status.MigrationsFailed in
      notify_about_failure db_label err
    in
    Lwt.catch
      (fun () ->
        Migration.execute db_label (Pool_database.Tenant.steps ())
        |>> (fun () -> set_status Status.Active)
        >|> function
        | Ok () -> Lwt.return ()
        | Error err -> handle_error err)
      (fun exn ->
        let exn = Printexc.to_string exn in
        let err = Pool_message.Error.MigrationFailed exn in
        handle_error err)
  in
  db_labels |> Lwt_list.iter_s run
;;

let tenants =
  Command_utils.make_no_args
    "migrate.tenant"
    "Migrate tenant databases"
    (fun () ->
       let (_ : status) = Root.add () in
       let%lwt db_pools = Tenant.setup () in
       let%lwt () = migrate_tenants db_pools in
       Lwt.return_some ())
;;

let tenant_migration_pending =
  Command_utils.make_no_args
    "migrate.tenant_migrations_pending"
    "Set tenant database status to migration pending"
    (fun () ->
       let (_ : status) = Root.add () in
       let%lwt db_pools = Tenant.setup () in
       let%lwt () = Database.Tenant.set_migration_pending db_pools in
       let%lwt () = exit 0 in
       Lwt.return_some ())
;;
