open CCFun.Infix
include Migration_entity

let src = Logs.Src.create "database.migration"

module Logs = (val Logs.src_log src : Logs.LOG)
module Map = CCMap.Make (String)

let registered_migrations : steps Map.t ref = ref Map.empty

type config = { migration_state_table : string option }

let config migration_state_table = { migration_state_table }

let schema =
  let open Conformist in
  make
    Field.
      [ Conformist.optional
          (string ~default:"core_migration_state" "MIGRATION_STATE_TABLE")
      ]
    config
;;

let table () =
  Option.value
    ~default:"core_migration_state"
    (Sihl.Configuration.read schema).migration_state_table
;;

let raise_error label =
  Service.raise_caqti_error ~tags:(Logger.Tags.create label)
;;

let setup label () =
  Logs.debug (fun m -> m "Setting up table if not exists");
  Migration_repo.create_table_if_not_exists label (table ())
;;

let has database_label namespace =
  Migration_repo.get database_label (table ()) ~namespace
  |> Lwt.map Option.is_some
;;

let get database_label namespace =
  let%lwt state = Migration_repo.get database_label (table ()) ~namespace in
  Lwt.return
  @@
  match state with
  | Some state -> state
  | None ->
    raise
      (Exception
         (Printf.sprintf "Could not get migration state for %s" namespace))
;;

let upsert database_label state =
  Migration_repo.upsert database_label (table ()) state
;;

let mark_dirty database_label namespace =
  let%lwt state = get database_label namespace in
  let dirty_state = Migration_repo.Migration.mark_dirty state in
  let%lwt () = upsert database_label dirty_state in
  Lwt.return dirty_state
;;

let mark_clean database_label namespace =
  let%lwt state = get database_label namespace in
  let clean_state = Migration_repo.Migration.mark_clean state in
  let%lwt () = upsert database_label clean_state in
  Lwt.return clean_state
;;

let increment database_label namespace =
  let%lwt state = get database_label namespace in
  let updated_state = Migration_repo.Migration.increment state in
  let%lwt () = upsert database_label updated_state in
  Lwt.return updated_state
;;

let register_migration migration =
  let label, _ = migration in
  let found = Map.find_opt label !registered_migrations in
  match found with
  | Some _ ->
    Logs.debug (fun m -> m "Found duplicate migration '%s', ignoring it" label)
  | None ->
    registered_migrations
    := Map.add label (snd migration) !registered_migrations
;;

let register_migrations migrations = CCList.iter register_migration migrations

let set_fk_check_request =
  let open Caqti_request.Infix in
  "SET FOREIGN_KEY_CHECKS = ?" |> Caqti_type.(bool ->. unit)
;;

let with_disabled_fk_check database_label f =
  Service.query database_label (fun connection ->
    let module Connection = (val connection : Caqti_lwt.CONNECTION) in
    let%lwt () =
      Connection.exec set_fk_check_request false
      |> Lwt.map (raise_error database_label)
    in
    Lwt.finalize
      (fun () -> f connection)
      (fun () ->
        Connection.exec set_fk_check_request true
        |> Lwt.map (raise_error database_label)))
;;

let execute_steps database_label migration =
  let open Caqti_request.Infix in
  let tags = Logger.Tags.create database_label in
  let namespace, steps = migration in
  let rec run steps =
    match steps with
    | [] -> Lwt.return ()
    | { Step.label; statement; check_fk = true } :: steps ->
      Logs.debug (fun m -> m ~tags "Running %s" label);
      let query (module Connection : Caqti_lwt.CONNECTION) =
        let req = statement |> Caqti_type.(unit ->. unit) ~oneshot:true in
        Connection.exec req ()
      in
      let%lwt () =
        Service.query database_label query
        |> Lwt.map (raise_error database_label)
      in
      Logs.debug (fun m -> m ~tags "Ran %s" label);
      let%lwt _ = increment database_label namespace in
      run steps
    | { Step.label; statement; check_fk = false } :: steps ->
      let%lwt () =
        with_disabled_fk_check database_label (fun connection ->
          Logs.debug (fun m -> m ~tags "Running %s without fk checks" label);
          let query (module Connection : Caqti_lwt.CONNECTION) =
            let req = statement |> Caqti_type.(unit ->. unit) ~oneshot:true in
            Connection.exec req ()
          in
          query connection)
        |> Lwt.map (raise_error database_label)
      in
      Logs.debug (fun m -> m ~tags "Ran %s" label);
      let%lwt _ = increment database_label namespace in
      run steps
  in
  let () =
    match CCList.length steps with
    | 0 ->
      Logs.debug (fun m -> m ~tags "No migrations to apply for %s" namespace)
    | n ->
      Logs.debug (fun m -> m ~tags "Applying %i migrations for %s" n namespace)
  in
  run steps
;;

let execute_migration database_label migration =
  let tags = Logger.Tags.create database_label in
  let namespace, _ = migration in
  let%lwt () = setup database_label () in
  let%lwt has_state = has database_label namespace in
  let%lwt state =
    if has_state
    then (
      let%lwt state = get database_label namespace in
      if Migration_repo.Migration.dirty state
      then (
        Logs.err (fun m ->
          m
            ~tags
            "Dirty migration found for %s, this has to be fixed manually"
            namespace);
        Logs.info (fun m ->
          m
            ~tags
            "Set the column 'dirty' from 1/true to 0/false after you have \
             fixed the database state.");
        raise Dirty_migration)
      else mark_dirty database_label namespace)
    else (
      Logs.debug (fun m -> m ~tags "Setting up table for %s" namespace);
      let state = Migration_repo.Migration.create ~namespace in
      let%lwt () = upsert database_label state in
      Lwt.return state)
  in
  let migration_to_apply =
    Migration_repo.Migration.steps_to_apply migration state
  in
  let n_migrations = CCList.length (snd migration_to_apply) in
  if n_migrations > 0
  then
    Logs.info (fun m ->
      m
        ~tags
        "Executing %d migrations for '%s'..."
        (CCList.length (snd migration_to_apply))
        namespace)
  else
    Logs.info (fun m -> m ~tags "No migrations to execute for '%s'" namespace);
  let%lwt () =
    Lwt.catch
      (fun () -> execute_steps database_label migration_to_apply)
      (fun exn ->
        let err = Printexc.to_string exn in
        Logs.err (fun m ->
          m ~tags "Error while running migration '%a': %s" pp migration err);
        raise (Exception err))
  in
  let%lwt _ = mark_clean database_label namespace in
  Lwt.return ()
;;

let execute database_label migrations =
  let tags = Logger.Tags.create database_label in
  let n = CCList.length migrations in
  if n > 0
  then
    Logs.info (fun m ->
      m ~tags "Looking at %i migrations" (CCList.length migrations))
  else Logs.info (fun m -> m ~tags "No migrations to execute");
  let rec run migrations =
    match migrations with
    | [] -> Lwt.return ()
    | migration :: migrations ->
      let%lwt () = execute_migration database_label migration in
      run migrations
  in
  run migrations
;;

let run_all database_label () =
  let steps = !registered_migrations |> Map.to_seq |> CCList.of_seq in
  execute database_label steps
;;

let migrations_status ?migrations database_label () =
  let migrations_to_check =
    match migrations with
    | Some migrations -> migrations |> CCList.to_seq |> Map.of_seq
    | None -> !registered_migrations
  in
  let%lwt migrations_states =
    Migration_repo.get_all database_label (table ())
  in
  let migration_states_namespaces =
    migrations_states |> CCList.map Migration_repo.Migration.namespace
  in
  let registered_migrations_namespaces =
    Map.to_seq migrations_to_check |> CCList.of_seq |> CCList.map fst
  in
  let namespaces_to_check =
    CCList.concat
      [ migration_states_namespaces; registered_migrations_namespaces ]
    |> CCList.uniq ~eq:String.equal
  in
  Lwt.return
  @@ CCList.map
       (fun namespace ->
         let migrations = Map.find_opt namespace migrations_to_check in
         let migration_state =
           CCList.find_opt
             (Migration_repo.Migration.namespace %> CCString.equal namespace)
             migrations_states
         in
         match migrations, migration_state with
         | None, None -> namespace, None
         | None, Some migration_state ->
           namespace, Some (-Migration_repo.Migration.version migration_state)
         | Some migrations, Some migration_state ->
           let unapplied_migrations_count =
             CCList.length migrations
             - Migration_repo.Migration.version migration_state
           in
           namespace, Some unapplied_migrations_count
         | Some migrations, None -> namespace, Some (CCList.length migrations))
       namespaces_to_check
;;

let pending_migrations database_label () =
  let%lwt unapplied = migrations_status database_label () in
  let rec find_pending result = function
    | (namespace, Some n) :: xs ->
      if n > 0
      then (
        let result = CCList.cons (namespace, n) result in
        find_pending result xs)
      else find_pending result xs
    | (_, None) :: xs -> find_pending result xs
    | [] -> result
  in
  Lwt.return @@ find_pending [] unapplied
;;

let check_migrations_status ?migrations database_label () =
  let tags = Logger.Tags.create database_label in
  let%lwt unapplied = migrations_status database_label ?migrations () in
  CCList.iter
    (fun (namespace, count) ->
      match count with
      | None ->
        Logs.warn (fun m ->
          m
            ~tags
            "Could not find registered migrations for namespace '%s'. This \
             implies you removed all migrations of that namespace. Migrations \
             should be append-only. If you intended to remove those \
             migrations, make sure to remove the migration state in your \
             database/other persistence layer."
            namespace)
      | Some count ->
        if count > 0
        then
          Logs.info (fun m ->
            m
              ~tags
              "Unapplied migrations for namespace '%s' detected. Found %s \
               unapplied migrations, run command 'migrate'."
              namespace
              (Int.to_string count))
        else if count < 0
        then
          Logs.warn (fun m ->
            m
              ~tags
              "Fewer registered migrations found than migration state \
               indicates for namespace '%s'. Current migration state version \
               is ahead of registered migrations by %s. This implies you \
               removed migrations, which should be append-only."
              namespace
              (Int.to_string @@ Int.abs count))
        else ())
    unapplied;
  Lwt.return ()
;;

let start database_label () =
  Sihl.Configuration.require schema;
  let%lwt () = setup database_label () in
  let skip_default_pool_creation =
    Option.value
      ~default:false
      (Sihl.Configuration.read_bool "DATABASE_SKIP_DEFAULT_POOL_CREATION")
  in
  if Sihl.Configuration.is_test () || skip_default_pool_creation
  then Lwt.return ()
  else check_migrations_status database_label ()
;;

let extend_migrations additional_steps () =
  let registered_migrations = !registered_migrations in
  let migrations = (registered_migrations |> Map.to_list) @ additional_steps in
  match
    CCList.(length migrations == length (uniq ~eq:Utils.equal_key migrations))
  with
  | true -> migrations
  | false ->
    Logs.info (fun m ->
      m
        "There are duplicated migrations: %s\nRemove or rename them."
        (CCList.fold_left
           (fun a b -> Format.asprintf "%s\n%s" a (fst b))
           ""
           (CCList.stable_sort
              (fun a b -> CCString.compare (fst a) (fst b))
              migrations)));
    []
;;

let run_pending_migrations db_pools migration_steps =
  let%lwt status =
    Lwt_list.map_s
      (fun label ->
        let%lwt m = pending_migrations label () in
        (label, m) |> Lwt.return)
      db_pools
  in
  Lwt_list.iter_s
    (fun (label, pending_migrations) ->
      let tags = Logger.Tags.create label in
      let msg prefix =
        Format.asprintf
          "%s pending migration for database pool: %a"
          prefix
          Entity.Label.pp
          label
      in
      if CCList.length pending_migrations > 0
      then (
        Logs.debug (fun m -> m ~tags "%s" @@ msg "Run");
        execute label migration_steps)
      else (
        Logs.debug (fun m -> m ~tags "%s" @@ msg "No");
        Lwt.return_unit))
    status
;;
