open Database
open Utils.Lwt_result.Infix

module Outbox : sig
  type t = string * Email.Service.Smtp.prepared

  val add : t -> unit
  val send : unit -> unit Lwt.t
end = struct
  type t = string * Email.Service.Smtp.prepared

  let notifications : t list ref = ref []
  let add notification = notifications := notification :: !notifications

  let send_notifications () =
    Logs.info (fun m ->
      m "Sending %i migration notifications" (List.length !notifications));
    let send
          (sender, { Email.Service.Smtp.reply_to; recipients; subject; body; config; _ })
      =
      Letters.create_email ~reply_to ~from:sender ~recipients ~subject ~body ()
      |> function
      | Ok message -> Letters.send ~config ~sender ~recipients ~message
      | Error msg -> raise (Sihl.Contract.Email.Exception msg)
    in
    !notifications
    |> Lwt_list.iter_s (fun notification ->
      Lwt.catch
        (fun () -> send notification)
        (fun exn ->
           let err = Printexc.to_string exn in
           let error_message =
             Format.asprintf "Error while  notifying about failed migration '%s'" err
           in
           Logs.err (fun m -> m "%s" error_message) |> Lwt.return))
  ;;

  let send () =
    if Sihl.Configuration.is_development ()
    then Lwt.return_unit
    else send_notifications ()
  ;;
end

let root =
  Command_utils.make_no_args "migrate.root" "Migrate root database" (fun () ->
    let%lwt () = Pool.Root.setup () in
    let%lwt () =
      Migration.execute Pool.Root.label (Pool_database.Root.steps ())
      ||> function
      | Ok () -> ()
      | Error _ -> exit 1
    in
    Lwt.return_some ())
;;

let create_failure_notification db_label error =
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
    let%lwt prepared = Email.Service.Smtp.prepare Pool.Root.label email in
    Outbox.add (sender, prepared) |> Lwt.return
  | _ -> Lwt.return_unit
;;

let migrate_tenants db_labels =
  let open Utils.Lwt_result.Infix in
  let run db_label =
    let set_status status =
      Pool_database.(StatusUpdated (db_label, status) |> handle_event Pool.Root.label)
    in
    let handle_error err =
      let err = Pool_common.(Utils.error_to_string Language.En err) in
      let%lwt () = set_status Status.MigrationsFailed in
      create_failure_notification db_label err
    in
    match%lwt Pool.Tenant.test_connection db_label with
    | Ok () ->
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
    | Error _ -> set_status Status.MigrationsConnectionIssue
  in
  db_labels |> Lwt_list.iter_p run >|> Outbox.send
;;

let tenants =
  Command_utils.make_no_args "migrate.tenant" "Migrate tenant databases" (fun () ->
    let%lwt () = Pool.Root.setup () in
    let%lwt db_pools = Pool.Tenant.setup () in
    let%lwt () = migrate_tenants db_pools in
    Lwt.return_some ())
;;

let tenant_migration_pending =
  Command_utils.make_no_args
    "migrate.tenant_migrations_pending"
    "Set tenant database status to migration pending"
    (fun () ->
       let () = Pool.Root.add () in
       let%lwt db_pools = Pool.Tenant.setup () in
       let%lwt () = Pool.Tenant.set_migration_pending db_pools in
       Lwt.return_some ())
;;

module YojsonTupleMigration = struct
  open Caqti_request.Infix

  let select_request =
    {sql|
      SELECT
        uuid,
        changes
      FROM pool_change_log
    |sql}
    |> Caqti_type.(unit ->* t2 string string)
  ;;

  let update_request =
    {sql|
      UPDATE pool_change_log
      SET changes = ?
      WHERE uuid = ?
    |sql}
    |> Caqti_type.(t2 string string ->. unit)
  ;;

  let rec transform_tuples (json : Yojson.Safe.t) : Yojson.Safe.t =
    match json with
    | `Tuple items -> `List (CCList.map transform_tuples items)
    | `List items -> `List (CCList.map transform_tuples items)
    | `Assoc pairs -> `Assoc (CCList.map (fun (k, v) -> k, transform_tuples v) pairs)
    | other -> other
  ;;

  let migrate_changelog database_label =
    let tags = Database.Logger.Tags.create database_label in
    Logs.info (fun m ->
      m ~tags "Starting yojson tuple migration for pool_change_log table");
    let%lwt changelogs = Database.collect database_label select_request () in
    let total = CCList.length changelogs in
    Logs.info (fun m -> m ~tags "Found %d changelog entries to process" total);
    let%lwt migrated_count =
      Lwt_list.fold_left_s
        (fun count (uuid, changes_str) ->
           try
             let json = Yojson.Safe.from_string changes_str in
             let transformed = transform_tuples json in
             if Yojson.Safe.equal json transformed
             then Lwt.return count
             else (
               let new_changes_str = Yojson.Safe.to_string transformed in
               let%lwt () =
                 Database.exec database_label update_request (new_changes_str, uuid)
               in
               Lwt.return (count + 1))
           with
           | exn ->
             Logs.warn (fun m ->
               m ~tags "Failed to process changelog %s: %s" uuid (Printexc.to_string exn));
             Lwt.return count)
        0
        changelogs
    in
    Logs.info (fun m ->
      m ~tags "Completed migration: %d/%d entries updated" migrated_count total);
    Lwt.return_unit
  ;;

  let run_all () =
    let () = Pool.Root.add () in
    let%lwt db_pools = Pool.Tenant.setup () in
    Lwt_list.iter_s migrate_changelog db_pools
  ;;
end

let migrate_yojson_tuples =
  Command_utils.make_no_args
    "migrate.yojson_tuples"
    "Migrate json serialized data in the database to use lists rather than tuples due to \
     removal of tuples in yojson 3.0.0+"
    (fun () ->
       let%lwt () = YojsonTupleMigration.run_all () in
       Lwt.return_some ())
;;
