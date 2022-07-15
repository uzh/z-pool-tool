let setup_databases () =
  Database.Root.setup ();
  Database.Tenant.setup ()
;;
