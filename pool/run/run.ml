let commands =
  let open Command in
  [ Migrate.root
  ; Migrate.tenants
  ; Seed.root_data
  ; Seed.root_data_clean
  ; Seed.tenant_data
  ; Seed.tenant_data_clean
  ; Tenant_pool.create_tenant_pool
  ; Participant.sign_up
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
    |> before_start (fun () -> Printexc.record_backtrace true |> Lwt.return)
    |> run ~commands)
;;
