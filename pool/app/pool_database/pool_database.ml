module Root = Root
module Tenant = Tenant
open Utils.Lwt_result.Infix

let src = Logs.Src.create "database"

module Logs = (val Logs.src_log src : Logs.LOG)

let connect_and_migrate database_label =
  match%lwt Database.connect database_label with
  | Ok () ->
    let steps () =
      if Database.is_root database_label then Root.steps () else Tenant.steps ()
    in
    let open Database in
    let update_status = Tenant.update_status database_label in
    Migration.execute database_label (steps ())
    >|> (function
     | Ok () ->
       let%lwt () = update_status Status.Active in
       Lwt.return_ok ()
     | Error err ->
       let%lwt () = update_status Status.MigrationsFailed in
       Lwt.return_error err)
  | Error err -> Lwt.return_error err
;;

type event = Migrated of Database.t [@@deriving eq, show]

let root_steps = Root.steps
let tenant_steps = Tenant.steps

let handle_event _ : event -> unit Lwt.t = function
  | Migrated database ->
    let open Database in
    let label = database |> label in
    let tags = Logger.Tags.create label in
    Logs.info (fun m -> m ~tags "Migrating: %a" Label.pp label);
    let () = add_pool database in
    (match label |> is_root with
     | true -> root, root_steps ()
     | false -> label, tenant_steps ())
    |> CCFun.uncurry Database.Migration.execute
    >|- Pool_common.Utils.with_log_error ~src ~tags
    ||> (function
     | Ok () -> ()
     | Error err -> Pool_common.Utils.failwith err)
;;

let start () =
  let%lwt () = Database.Root.start () in
  let%lwt () = Root.start () in
  let%lwt () = Database.Tenant.start () in
  let%lwt () = Tenant.start () in
  Lwt.return_unit
;;

let stop () =
  let%lwt () = Tenant.stop () in
  let%lwt () = Database.Tenant.stop () in
  let%lwt () = Root.stop () in
  let%lwt () = Database.Root.stop () in
  Lwt.return_unit
;;

let lifecycle =
  Sihl.Container.create_lifecycle
    "database"
    ~implementation_name:"root and tenants incl. migrations"
    ~start
    ~stop
;;

let register () = Sihl.Container.Service.create lifecycle
