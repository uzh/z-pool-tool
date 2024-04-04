type event = Migrated of Database.t [@@deriving eq, show]

let handle_event _ : event -> unit Lwt.t = function
  | Migrated database ->
    let open Database in
    Logs.info (fun m -> m "Migrating: %s" (database |> label |> Label.show));
    let _ = add_pool database in
    (match database |> label |> is_root with
     | true -> Migration.Root.run ()
     | false -> Migration.Tenant.run [ database |> label ] ())
;;
