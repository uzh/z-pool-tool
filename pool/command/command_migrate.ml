open Database

let root =
  Command_utils.make_no_args "migrate.root" "Migrate root database" (fun () ->
    let (_ : status) = Root.add () in
    let%lwt () = Migration.execute root (Pool_database.Root.steps ()) in
    let () = exit 0 in
    Lwt.return_some ())
;;

let migrate_tenants db_labels =
  let notification db_label =
    let open Sihl_email in
    let read_config = Sihl.Configuration.read_string in
    let sender = read_config "SMTP_SENDER" in
    let recipient = read_config "SMTP_SENDER" in
    match sender, recipient with
    | Some sender, Some recipient ->
      let text =
        Format.asprintf
          {|Migrations on the tenant %s could not be executed successfully.

        Please take the necessary actions.|}
          (Database.Label.value db_label)
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
  let run db_label =
    let set_status = Database.Tenant.update_status db_label in
    Lwt.catch
      (fun () ->
        let%lwt () =
          Migration.execute db_label (Pool_database.Tenant.steps ())
        in
        set_status Status.Active)
      (fun _ ->
        let%lwt () = notification db_label in
        let%lwt () = set_status Status.MigrationsFailed in
        Lwt.return_unit)
  in
  let%lwt () = Database.Tenant.set_migration_pending db_labels in
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
