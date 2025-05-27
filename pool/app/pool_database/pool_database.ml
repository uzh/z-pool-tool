module Root = Root
module Tenant = Tenant
open Utils.Lwt_result.Infix
open Database

let src = Logs.Src.create "database"

module Logs = (val Logs.src_log src : Logs.LOG)

let connect_and_migrate database_label =
  match%lwt Pool.connect database_label with
  | Ok () ->
    let steps () =
      if Pool.is_root database_label then Root.steps () else Tenant.steps ()
    in
    let update_status = Pool.Tenant.update_status database_label in
    Migration.execute database_label (steps ())
    >|> (function
     | Ok () -> update_status Status.Active >|> Lwt.return_ok
     | Error err ->
       let%lwt () = update_status Status.MigrationsFailed in
       Lwt.return_error err)
  | Error err -> Lwt.return_error err
;;

type event =
  | Migrated of t
  | StatusUpdated of Label.t * Status.t
[@@deriving eq, show]

let root_steps = Root.steps
let tenant_steps = Tenant.steps

let handle_event _ : event -> unit Lwt.t = function
  | Migrated database ->
    let label = database |> label in
    let tags = Logger.Tags.create label in
    Logs.info (fun m -> m ~tags "Migrating: %a" Label.pp label);
    (match label |> Pool.is_root with
     | true ->
       let () = Pool.Root.add () in
       Pool.Root.label, root_steps ()
     | false ->
       let label = Pool.Tenant.add database in
       label, tenant_steps ())
    |> CCFun.uncurry Migration.execute
    >|- Pool_common.Utils.with_log_error ~src ~tags
    ||> (function
     | Ok () -> ()
     | Error err -> Pool_common.Utils.failwith err)
  | StatusUpdated (database_label, status) ->
    Pool.Tenant.update_status database_label status
;;

let start () =
  let%lwt () = Pool.Root.start () in
  let%lwt () = Root.start () in
  let%lwt () = Pool.Tenant.start () in
  let%lwt () = Tenant.start () in
  Lwt.return_unit
;;

let stop () =
  let%lwt () = Tenant.stop () in
  let%lwt () = Pool.Tenant.stop () in
  let%lwt () = Root.stop () in
  let%lwt () = Pool.Root.stop () in
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
