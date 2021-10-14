type config =
  { url : string
  ; pool_size : int option
  ; skip_default_pool_creation : bool option
  ; choose_pool : string option
  }

let config url pool_size skip_default_pool_creation choose_pool =
  { url; pool_size; skip_default_pool_creation; choose_pool }
;;

let schema =
  Conformist.(
    make
      Field.
        [ string
            ~meta:
              "The root database connection url. This is the only string that \
               Sihl needs to connect to a database."
            "DATABASE_URL"
        ; Conformist.optional
            ~meta:
              "The amount of connections in the database connection pool that \
               Sihl manages. If the number is too high, the server might \
               struggle. If the number is too low, your Sihl app performs \
               badly. This can be configured using DATABASE_POOL_SIZE and the \
               default is 10."
            (int ~default:10 "DATABASE_POOL_SIZE")
        ; Conformist.optional
            ~meta:
              "This value is by default set to [true] to skip the creation of \
               the default connection pool. This is useful if an application \
               uses multiple databases."
            (bool ~default:true "DATABASE_SKIP_DEFAULT_POOL_CREATION")
        ; Conformist.optional
            ~meta:
              "The database connection pool name that should be used by \
               default. By default ['root'] is used for this application."
            (string
               ~default:Pool_common.Database.(Label.value root)
               "DATABASE_CHOOSE_POOL")
        ]
      config)
;;

module Root = struct
  let label = Pool_common.Database.(Label.value root)

  module Migration = struct
    include Migration.Root
  end

  module Seed = struct
    include Seed.Root
  end

  let setup () =
    Sihl.Database.add_pool label (Sihl.Configuration.read schema).url
  ;;
end

module Tenant = struct
  let label = "tenant"

  module Migration = struct
    include Migration.Tenant
  end

  module Seed = struct
    include Seed.Tenant
  end

  let setup () =
    let%lwt tenants = Tenant.find_databases () in
    match tenants with
    | Error err -> failwith err
    | Ok [] -> failwith "There are no tenants registered in root database!"
    | Ok tenants ->
      CCList.map
        (fun pool ->
          let open Pool_common.Database in
          add_pool pool;
          pool.label)
        tenants
      |> Lwt.return
  ;;
end

let start () =
  Logs.info (fun m -> m "Start database %s" Root.label);
  Root.setup ();
  let%lwt () =
    Service.Migration.check_migrations_status
      ~ctx:[ "pool", Root.label ]
      ~migrations:(Root.Migration.steps ())
      ()
  in
  let%lwt db_pools = Tenant.setup () in
  Lwt_list.iter_s
    (fun db_pool ->
      let db_pool = Pool_common.Database.Label.value db_pool in
      Logs.info (fun m -> m "Start database %s" db_pool);
      Service.Migration.check_migrations_status
        ~ctx:[ "pool", db_pool ]
        ~migrations:(Tenant.Migration.steps ())
        ())
    db_pools
;;

let stop () = Lwt.return_unit

let lifecycle =
  Sihl.Container.create_lifecycle
    "database"
    ~implementation_name:"root and tenants"
    ~start
    ~stop
;;

let register () =
  let configuration = Sihl.Configuration.make ~schema () in
  Sihl.Container.Service.create ~configuration lifecycle
;;
