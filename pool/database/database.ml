open CCFun.Infix

let src = Logs.Src.create "database"

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

  module Migration = Migration.Root

  let add () =
    let open CCResult in
    let open Pool_database in
    let open Pool_common.Utils in
    let label = Label.of_string label in
    (Sihl.Configuration.read schema).url
    |> Url.create
    >>= create label
    |> get_or_failwith
    |> add_pool
  ;;

  let setup = add %> Lwt.return
end

module Tenant = struct
  let label = "tenant"

  module Migration = Migration.Tenant

  let setup_tenant ?(run_functions = []) database =
    let open Pool_database in
    add_pool database;
    let%lwt () =
      Lwt_list.iter_s
        (fun (fcn : ?ctx:(string * string) list -> unit -> unit Lwt.t) ->
          fcn ~ctx:(to_ctx database.label) ())
        run_functions
    in
    Lwt.return database.label
  ;;

  let setup ?run_functions () =
    match%lwt Pool_tenant.find_databases () with
    | [] ->
      let open Pool_common in
      failwith (Message.NoTenantsRegistered |> Utils.error_to_string Language.En)
    | tenants -> Lwt_list.map_s (setup_tenant ?run_functions) tenants
  ;;
end

type event = Migrated of Pool_database.t [@@deriving eq, show]

let handle_event _ : event -> unit Lwt.t = function
  | Migrated database ->
    let open Pool_database in
    Logs.info (fun m -> m "Migrating: %s" (Label.show database.label));
    add_pool database;
    (match is_root database.label with
     | true -> Migration.Root.run ()
     | false -> Migration.Tenant.run [ database.label ] ())
;;

let start () =
  let tags = Pool_database.Logger.Tags.create Pool_database.root in
  Logs.info ~src (fun m -> m ~tags "Start database %s" Root.label);
  let ctx = [ "pool", Root.label ] in
  let%lwt () = Root.setup () in
  let%lwt () =
    Service.Migration.check_migrations_status
      ~ctx
      ~migrations:(Root.Migration.steps ())
      ()
  in
  let%lwt db_pools = Tenant.setup () in
  Lwt_list.iter_s
    (fun pool ->
      Logs.info ~src (fun m ->
        m ~tags "Start database %s" (Pool_database.Label.value pool));
      let ctx = Pool_database.to_ctx pool in
      Service.Migration.check_migrations_status
        ~ctx
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
