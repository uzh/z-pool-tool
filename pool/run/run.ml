let commands =
  let open Command in
  [ Migrate.root
  ; Migrate.tenants
  ; Seed.root_data
  ; Seed.root_data_clean
  ; Seed.tenant_data
  ; Seed.tenant_data_clean
  ; Tenant_pool.create_tenant_pool
  ; Contact.sign_up
  ; SessionReminder.all_tenants_session_reminder
  ; SessionReminder.tenant_specific_session_reminder
  ]
;;

let services =
  [ Database.register ()
  ; Service.User.register ()
  ; Service.Token.register ()
  ; Service.BlockingEmail.register ()
  ; Service.Email.register ()
  ; Service.EmailTemplate.register ()
  ; Service.Queue.register ()
  ; Service.Storage.register ()
  ; Sihl.Web.Http.register ~middlewares:Routes.global_middlewares Routes.router
  ]
;;

(* This is the entry point of your Sihl app *)
let () =
  Sihl.App.(
    empty
    |> with_services services
    |> before_start (fun () ->
         let () = Middleware.Error.before_start () in
         (* put_perms will fail on permissions that already exist, so we need to
            clear the root database first in order to make sure new
            [root_permissions] items end up in the database *)
         let%lwt () =
           (* TODO: Handle errors *)
           Lwt_list.fold_left_s
             (fun _acc perm ->
               let%lwt _rv = Ocauth.Persistence.delete_perm perm in
               Lwt.return ())
             ()
             Ocauth.root_permissions
         in
         let%lwt (_
                   : ( Ocauth.Persistence.auth_rule list
                     , Ocauth.Persistence.auth_rule list )
                     result)
           =
           Ocauth.(Persistence.put_perms root_permissions)
         in
         Printexc.record_backtrace true |> Lwt.return)
    |> run ~commands)
;;
