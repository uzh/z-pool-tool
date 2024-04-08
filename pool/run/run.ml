let src = Logs.Src.create "run"
let () = Printexc.record_backtrace true

let worker_services =
  [ Pool_canary.register ()
  ; Database.register ()
  ; Service.Storage.register ()
  ; Schedule.register ()
  ; Queue.register
      ~jobs:
        [ Queue.hide Email.Service.Job.send
        ; Queue.hide Text_message.Service.Job.send
        ; Queue.hide Assignment_job.job
        ]
      ()
  ; Matcher.register ()
  ; System_event.Service.register `Worker ()
  ; User_import.Service.register ()
  ; Reminder.Service.register ()
  ]
;;

let services =
  [ Pool_canary.register ()
  ; Database.register ()
  ; Service.User.register ~commands:[] ()
  ; Service.Token.register ()
  ; Email.Service.register ()
  ; Text_message.Service.register ()
  ; Email.Service.Queue.register ()
  ; Service.Storage.register ()
  ; Sihl.Web.Http.register ~middlewares:Routes.global_middlewares Routes.router
  ; System_event.Service.register `Server ()
  ; Assignment_job.register ()
  ]
;;

let commands =
  let open Command in
  [ Migrate.root
  ; Migrate.tenants
  ; Seed.root_data
  ; Seed.root_data_clean
  ; Seed.tenant_data
  ; Seed.tenant_data_clean
  ; Seed.tenant_data_test
  ; Seed.tenant_data_clean_specific
  ; Seed.tenant_data_contacts_specific
  ; Tenant_pool.create_tenant_pool
  ; Tenant_pool.update_tenant_database_url
  ; Contact.sign_up
  ; SessionReminder.all_tenants_session_reminder
  ; SessionReminder.tenant_specific_session_reminder
  ; SystemEvent.handle_system_events_command
  ; Contact.all_profile_update_triggers
  ; Contact.tenant_specific_profile_update_trigger
  ; Matcher.run_tenant
  ; Matcher.run_all
  ; Mail.send_mail
  ; Admin.create
  ; Admin.grant_role
  ; Admin.list_roles
  ; Admin.create_root_admin
  ; Utils.encrypt_string
  ; UserImport.handle_user_import_notifications
  ; DefaultData.insert
  ; Worker.run ~services:worker_services ()
  ; version
  ]
;;

(* This is the entry point of your Sihl app *)
let () =
  Sihl.App.(
    empty
    |> with_services services
    |> before_start (fun () ->
      (Lwt.async_exception_hook
       := fun exn ->
            Pool_common.Message.NotHandled (Printexc.to_string exn)
            |> Pool_common.Utils.with_log_error ~src
            |> ignore);
      Lwt.return @@ Logger.create_logs_dir ())
    |> run ~commands ~log_reporter:Logger.reporter)
;;
