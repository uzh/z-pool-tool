module Queue = Email.Service.Queue

let create_queues =
  Command_utils.make_no_args "queue.create" "Create rabbitmq queues" (fun () ->
    let%lwt () = Database.Pool.Root.setup () in
    let%lwt db_pools = Database.Pool.Tenant.setup () in
    let%lwt (_ : Amqp_client_lwt.Queue.t option list) =
      Lwt_list.map_s Queue.Queue.create (Database.Pool.Root.label :: db_pools)
    in
    Lwt.return_some ())
;;

let migrate_queues =
  Command_utils.make_no_args "queue.migrate" "Migrate rabbitmq queues" (fun () ->
    let%lwt () = Database.Pool.Root.setup () in
    let%lwt db_pools = Database.Pool.Tenant.setup () in
    let%lwt () = Lwt_list.iter_s Queue.Queue.update db_pools in
    Lwt.return_some ())
;;
