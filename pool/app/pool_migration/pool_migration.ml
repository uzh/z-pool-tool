let src = Logs.Src.create "migration"

module Logs = (val Logs.src_log src : Logs.LOG)
module Root = Root
module Tenant = Tenant

type event = Migrated of Database.t [@@deriving eq, show]

let root_steps = Root.steps
let tenant_steps = Tenant.steps

let handle_event _ : event -> unit Lwt.t = function
  | Migrated database ->
    let open Database in
    let label = database |> label in
    Logs.info (fun m ->
      m ~tags:(Logger.Tags.create label) "Migrating: %a" Label.pp label);
    let (_ : status) = add_pool database in
    (match label |> is_root with
     | true -> root, root_steps ()
     | false -> label, tenant_steps ())
    |> CCFun.uncurry Database.Migration.execute
;;
