let () = Printexc.record_backtrace true

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
  ]
;;

let services =
  [ Database.register ()
  ; Service.User.register ~commands:[] ()
  ; Service.Token.register ()
  ; Service.BlockingEmail.register ()
  ; Service.Email.register ()
  ; Service.EmailTemplate.register ()
  ; Service.Queue.register ()
  ; Service.Storage.register ()
  ; Matcher.register ()
  ; Sihl.Web.Http.register ~middlewares:Routes.global_middlewares Routes.router
  ]
;;

(* This is the entry point of your Sihl app *)
let () =
  Sihl.App.(
    empty
    |> with_services services
    |> before_start (fun () ->
         Logger.create_logs_dir ();
         Lwt.return @@ Middleware.Error.before_start ())
    |> run ~commands ~log_reporter:Logger.reporter)
;;
