let src = Logs.Src.create "run"
let () = Printexc.record_backtrace true

let worker_services =
  [ Database.register ()
  ; Service.Storage.register ()
  ; Schedule.register ()
  ; Queue.register ~jobs:[ Queue.hide Pool_tenant.Service.Email.Job.send ] ()
  ; Matcher.register ()
  ]
;;

let services =
  [ Database.register ()
  ; Service.User.register ~commands:[] ()
  ; Service.Token.register ()
  ; Pool_tenant.Service.Email.register ()
  ; Pool_tenant.Service.Queue.register ()
  ; Service.Storage.register ()
  ; Sihl.Web.Http.register ~middlewares:Routes.global_middlewares Routes.router
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
  ; Seed.tenant_data_clean_specific
  ; Seed.tenant_data_contacts_specific
  ; Seed.tenant_seed_default
  ; Tenant_pool.create_tenant_pool
  ; Tenant_pool.update_tenant_database_url
  ; Contact.sign_up
  ; SessionReminder.all_tenants_session_reminder
  ; SessionReminder.tenant_specific_session_reminder
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
         Logger.create_logs_dir ();
         Lwt.return @@ Middleware.Error.before_start ())
    |> run ~commands ~log_reporter:Logger.reporter)
;;
