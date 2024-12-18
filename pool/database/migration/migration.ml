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
  CCOption.value
    ~default:"core_migration_state"
    (Sihl.Configuration.read schema).migration_state_table
;;

let setup label () =
  Logs.debug (fun m -> m "Setting up table if not exists");
  Migration_repo.create_table_if_not_exists label (table ())
;;

let has database_label namespace =
  Migration_repo.get database_label (table ()) ~namespace |> Lwt.map CCOption.is_some
;;

let get_opt database_label namespace =
  Migration_repo.get database_label (table ()) ~namespace
;;

let get database_label namespace =
  let%lwt state = get_opt database_label namespace in
  Lwt.return
  @@
  match state with
  | Some state -> state
  | None ->
    raise (Exception (Printf.sprintf "Could not get migration state for %s" namespace))
;;

let upsert database_label state = Migration_repo.upsert database_label (table ()) state

let increment database_label state =
  let updated_state = Migration_repo.Migration.increment state in
  let%lwt () = upsert database_label updated_state in
  Lwt.return updated_state
;;

let register_migration migration =
  let label, _ = migration in
  let found = Map.find_opt label !registered_migrations in
  match found with
  | Some _ -> Logs.debug (fun m -> m "Found duplicate migration '%s', ignoring it" label)
  | None -> registered_migrations := Map.add label (snd migration) !registered_migrations
;;

let with_disabled_fk_check database_label f =
  let open Service in
  query database_label (fun connection ->
    let module Connection = (val connection : Caqti_lwt.CONNECTION) in
    let%lwt () =
      Connection.exec set_fk_check_request false |> Pool.raise_caqti_error database_label
    in
    Lwt.finalize
      (fun () -> f connection)
      (fun () ->
         Connection.exec set_fk_check_request true
         |> Pool.raise_caqti_error database_label))
;;

let execute_steps database_label state steps =
  let open Caqti_request.Infix in
  let tags = Logger.Tags.create database_label in
  let namespace = Migration_repo.Migration.namespace state in
  let rec run steps state =
    match steps with
    | [] -> Lwt.return state
    | { Step.label; statement; check_fk = true } :: steps ->
      Logs.debug (fun m -> m ~tags "Running %s" label);
      let query (module Connection : Caqti_lwt.CONNECTION) =
        let req = statement |> Caqti_type.(unit ->. unit) ~oneshot:true in
        Connection.exec req ()
      in
      let%lwt () = Service.query database_label query in
      Logs.debug (fun m -> m ~tags "Ran %s" label);
      let%lwt state = increment database_label state in
      run steps state
    | { Step.label; statement; check_fk = false } :: steps ->
      let%lwt () =
        with_disabled_fk_check database_label (fun connection ->
          Logs.debug (fun m -> m ~tags "Running %s without fk checks" label);
          let query (module Connection : Caqti_lwt.CONNECTION) =
            let req = statement |> Caqti_type.(unit ->. unit) ~oneshot:true in
            Connection.exec req ()
          in
          query connection)
      in
      Logs.debug (fun m -> m ~tags "Ran %s" label);
      let%lwt state = increment database_label state in
      run steps state
  in
  let () =
    match CCList.length steps with
    | 0 -> Logs.debug (fun m -> m ~tags "No migrations to apply for %s" namespace)
    | n -> Logs.debug (fun m -> m ~tags "Applying %i migrations for %s" n namespace)
  in
  run steps state
;;

let execute_migration database_label migration =
  let open Utils.Lwt_result.Infix in
  let tags = Logger.Tags.create database_label in
  let namespace, steps = migration in
  let%lwt () = setup database_label () in
  let upsert_state = upsert database_label in
  let%lwt existing_state = get_opt database_label namespace in
  let%lwt state, steps_to_apply =
    match existing_state with
    | Some state ->
      if Migration_repo.Migration.dirty state
      then (
        Logs.err (fun m ->
          m ~tags "Dirty migration found for %s, this has to be fixed manually" namespace);
        Logs.info (fun m ->
          m
            ~tags
            "Set the column 'dirty' from 1/true to 0/false after you have fixed the \
             database state.");
        raise Dirty_migration)
      else
        Lwt.return (state, Migration_repo.Migration.(steps_to_apply steps (version state)))
    | None ->
      (* If currently no state exists, an new one will be created. If there are steps to
         execute, it is dirty *)
      Logs.debug (fun m -> m ~tags "Setting up table for %s" namespace);
      let dirty = CCList.is_empty steps |> not in
      let state = Migration_repo.Migration.create ~namespace ~dirty in
      let%lwt () = upsert_state state in
      Lwt.return (state, steps)
  in
  let mark_as condition setter state =
    if condition
    then (
      let updated_state = setter state in
      let%lwt () = upsert_state updated_state in
      Lwt.return updated_state)
    else Lwt.return state
  in
  let mark_dirty state =
    let open Migration_repo.Migration in
    mark_as (state |> dirty |> not) mark_dirty state
  in
  let mark_clean state =
    let open Migration_repo.Migration in
    mark_as (state |> dirty) mark_clean state
  in
  match steps_to_apply with
  | [] ->
    Logs.info (fun m -> m ~tags "No migrations to execute for '%s'" namespace);
    let%lwt (_ : Migration_repo.Migration.t) = mark_clean state in
    Lwt_result.return ()
  | steps_to_apply ->
    Logs.info (fun m ->
      m
        ~tags
        "Executing %d migrations for '%s'..."
        (CCList.length steps_to_apply)
        namespace);
    let%lwt state = mark_dirty state in
    Lwt.catch
      (fun () ->
         execute_steps database_label state steps_to_apply
         >|> mark_clean
         ||> CCFun.const (CCResult.return ()))
      (fun exn ->
         let err = Printexc.to_string exn in
         let error_message =
           Format.asprintf "Error while running migration '%a': %s" pp migration err
         in
         Logs.err (fun m -> m ~tags "%s" error_message);
         Lwt_result.fail (Pool_message.Error.MigrationFailed error_message))
;;

let execute database_label migrations =
  let open Utils.Lwt_result.Infix in
  let tags = Logger.Tags.create database_label in
  let n = CCList.length migrations in
  if n > 0
  then Logs.info (fun m -> m ~tags "Looking at %i migrations" (CCList.length migrations))
  else Logs.info (fun m -> m ~tags "No migrations to execute");
  let rec run migrations =
    match migrations with
    | [] -> Lwt_result.return ()
    | migration :: migrations ->
      execute_migration database_label migration >>= fun () -> run migrations
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
  let%lwt migrations_states = Migration_repo.get_all database_label (table ()) in
  let migration_states_namespaces =
    migrations_states |> CCList.map Migration_repo.Migration.namespace
  in
  let registered_migrations_namespaces =
    Map.to_seq migrations_to_check |> CCList.of_seq |> CCList.map fst
  in
  let namespaces_to_check =
    CCList.concat [ migration_states_namespaces; registered_migrations_namespaces ]
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
              CCList.length migrations - Migration_repo.Migration.version migration_state
            in
            namespace, Some unapplied_migrations_count
          | Some migrations, None -> namespace, Some (CCList.length migrations))
       namespaces_to_check
;;

let pending_migrations ?migrations database_label () =
  let%lwt unapplied = migrations_status ?migrations database_label () in
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
             "Could not find registered migrations for namespace '%s'. This implies you \
              removed all migrations of that namespace. Migrations should be \
              append-only. If you intended to remove those migrations, make sure to \
              remove the migration state in your database/other persistence layer."
             namespace)
       | Some count ->
         if count > 0
         then
           Logs.info (fun m ->
             m
               ~tags
               "Unapplied migrations for namespace '%s' detected. Found %s unapplied \
                migrations, run command 'migrate'."
               namespace
               (Int.to_string count))
         else if count < 0
         then
           Logs.warn (fun m ->
             m
               ~tags
               "Fewer registered migrations found than migration state indicates for \
                namespace '%s'. Current migration state version is ahead of registered \
                migrations by %s. This implies you removed migrations, which should be \
                append-only."
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
    CCOption.value
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
  match CCList.(length migrations == length (uniq ~eq:Utils.equal_key migrations)) with
  | true -> migrations
  | false ->
    Logs.info (fun m ->
      m
        "There are duplicated migrations: %s\nRemove or rename them."
        (CCList.fold_left
           (fun a b -> Format.asprintf "%s\n%s" a (fst b))
           ""
           (CCList.stable_sort (fun a b -> CCString.compare (fst a) (fst b)) migrations)));
    []
;;
