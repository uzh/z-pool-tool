let src = Logs.Src.create "database"

module SeedAssets = Seed_assets

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
               badly. This can be configured using Pool_database_SIZE and the \
               default is 10."
            (int ~default:10 "Pool_database_SIZE")
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
               ~default:Pool_database.(Label.value root)
               "DATABASE_CHOOSE_POOL")
        ]
      config)
;;

module Root = struct
  let label = Pool_database.(Label.value root)

  module Migration = struct
    include Migration.Root
  end

  module Seed = struct
    include Seed.Root
  end

  let setup () =
    let open CCResult.Infix in
    let open Pool_database in
    let label = Label.create label |> Pool_common.Utils.get_or_failwith in
    (Sihl.Configuration.read schema).url
    |> Url.create
    >>= create label
    |> Pool_common.Utils.get_or_failwith
    |> add_pool
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
    let%lwt tenants = Pool_tenant.find_databases () in
    match tenants with
    | [] ->
      failwith
        Pool_common.(
          Message.NoTenantsRegistered |> Utils.error_to_string Language.En)
    | tenants ->
      CCList.map
        (fun pool ->
          let open Pool_database in
          add_pool pool;
          pool.label)
        tenants
      |> Lwt.return
  ;;
end

type event =
  | Added of Pool_database.t
  | Migrated of Pool_database.Label.t
[@@deriving eq, show]

let handle_event _ : event -> unit Lwt.t = function
  | Added pool ->
    Pool_database.add_pool pool;
    Lwt.return_unit
  | Migrated label ->
    let%lwt () =
      match Pool_database.is_root label with
      | true -> Migration.Root.run ()
      | false -> Migration.Tenant.run [ label ] ()
    in
    Lwt.return_unit
;;

let start () =
  let tags = Pool_database.Logger.Tags.create Pool_database.root in
  Logs.info ~src (fun m -> m ~tags "Start database %s" Root.label);
  Root.setup ();
  let%lwt () =
    Service.Migration.check_migrations_status
      ~ctx:[ "pool", Root.label ]
      ~migrations:(Root.Migration.steps ())
      ()
  in
  let%lwt db_pools = Tenant.setup () in
  Lwt_list.iter_s
    (fun pool ->
      Logs.info ~src (fun m ->
        m ~tags "Start database %s" (Pool_database.Label.value pool));
      Service.Migration.check_migrations_status
        ~ctx:(Pool_database.to_ctx pool)
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
