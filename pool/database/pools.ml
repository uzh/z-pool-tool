open CCFun
open Entity
open Utils.Lwt_result.Infix

exception Exception of string

let src = Logs.Src.create "pools"

module LogTag = struct
  let add_label : Label.t Logs.Tag.def =
    Logs.Tag.def "database_label" ~doc:"Database Label" Label.pp
  ;;

  let create database = Logs.Tag.(empty |> add add_label database)
end

let raise_caqti_error =
  let open Caqti_error in
  function
  | Ok resp -> resp
  | Error `Unsupported ->
    raise Pool_message.Error.(Exn (Unsupported "Caqti error"))
  | Error (#t as err) -> raise (Exn err)
;;

module Make (Config : Pools_sig.ConfigSig) = struct
  type connection =
    | Init
    | Pool of (Caqti_lwt.connection, Caqti_error.t) Caqti_lwt_unix.Pool.t
    | Fail of Caqti_error.load

  module Pool = struct
    type t =
      { database : Entity.t
      ; required : bool
      ; connection : connection [@opaque]
      ; n_retries : int
      }
    [@@deriving show, fields]

    let database_label { database; _ } = database |> label
    let database_url { database; _ } = database |> url

    let create ?(required = false) database =
      { database; required; connection = Init; n_retries = 0 }
    ;;

    let reset_retry pool = { pool with n_retries = 0 }
    let increment_retry pool = { pool with n_retries = pool.n_retries + 1 }

    let connect_pool =
      Url.to_uri
      %> Caqti_lwt_unix.connect_pool
           ~pool_config:
             (Caqti_pool_config.create ~max_size:Config.database_pool_size ())
    ;;

    let connect ?(retries = 2) ({ database; required; _ } as pool) =
      let tags = pool |> database_label |> LogTag.create in
      CCResult.retry retries (fun () -> pool |> database_url |> connect_pool)
      |> (function
            | Error [] ->
              raise
                Pool_message.Error.(
                  Exn (Unsupported "Failed to connect: empty error"))
            | Error (err :: _) when required -> raise_caqti_error (Error err)
            | Error (err :: _ as errors) ->
              Logs.warn ~src (fun m ->
                m
                  ~tags
                  "Failed to connect: %s (%s)"
                  (Entity.show database)
                  ([%show: Caqti_error.t list] errors));
              Fail err
            | Ok con -> Pool con)
      |> fun connection -> { pool with connection }
    ;;
  end

  module Config = Config

  module Cache = struct
    module Hashtbl = CCHashtbl.Make (Label)

    let pools : Pool.t Hashtbl.t =
      Hashtbl.create (max 1 Config.expected_databases)
    ;;

    let add = Hashtbl.add pools
    let remove = Hashtbl.remove pools
    let find_opt = Hashtbl.find_opt pools
    let replace pool = Hashtbl.replace pools (Pool.database_label pool) pool
  end

  let print_pool_usage ?tags =
    Pool.connection
    %> function
    | Pool pool ->
      let n_connections = Caqti_lwt_unix.Pool.size pool in
      let max_connections = Config.database_pool_size in
      Logs.debug ~src (fun m ->
        m ?tags "Pool usage: %i/%i" n_connections max_connections)
    | Init | Fail _ ->
      Logs.debug ~src (fun m -> m ?tags "Pool usage: No connection found")
  ;;

  let drain_opt =
    Pool.connection
    %> function
    | Pool pool -> Caqti_lwt_unix.Pool.drain pool
    | Init | Fail _ -> Lwt.return_unit
  ;;

  let add_pool ?required database =
    let label = database |> label in
    match Cache.find_opt label with
    | Some _ ->
      let msg =
        [%string "Failed to add pool: Pool already exists %{Label.show label}"]
      in
      Logs.err ~src (fun m -> m ~tags:(label |> LogTag.create) "%s" msg);
      failwith msg
    | None -> Pool.create ?required database |> Cache.add label
  ;;

  let drop_pool name =
    match Cache.find_opt name with
    | None ->
      let msg =
        [%string
          "Failed to drop pool: connection to '%{Label.show name}' doesn't \
           exist"]
      in
      Logs.info ~src (fun m -> m ~tags:(LogTag.create name) "%s" msg);
      Lwt.return_unit
    | Some pool ->
      let%lwt () = drain_opt pool in
      Cache.remove name |> Lwt.return
  ;;

  let initialize ?(additinal_pools : Entity.t list = []) () : unit =
    Config.database :: additinal_pools
    |> CCList.filter (label %> Cache.find_opt %> CCOption.is_none)
    |> CCList.iter (Pool.create ~required:true %> Cache.replace)
  ;;

  let connect =
    Cache.find_opt
    %> function
    | Some pool ->
      let rec connect pool =
        match pool.Pool.connection with
        | Fail err ->
          Error (Pool_message.Error.CaqtiError (Caqti_error.show err))
        | Init -> Pool.connect pool |> connect
        | Pool _ -> Ok ()
      in
      connect pool
    | None -> Error Pool_message.(Error.NotFound Field.Database)
  ;;

  let map_fetched ?(retries = 2) database_label =
    let rec run (fcn : 'a -> ('b, 'e) Lwt_result.t) =
      match Cache.find_opt database_label with
      | Some pool ->
        (match Pool.connection pool with
         | Fail err when pool.Pool.n_retries >= retries ->
           raise_caqti_error (Error err)
         | Fail _ ->
           let () =
             Pool.connect pool |> Pool.increment_retry |> Cache.replace
           in
           run fcn
         | Init ->
           let () = Pool.connect pool |> Cache.replace in
           run fcn
         | Pool connection when pool.Pool.n_retries > 0 ->
           let () = Pool.reset_retry pool |> Cache.replace in
           print_pool_usage pool;
           fcn connection
         | Pool connection ->
           print_pool_usage pool;
           fcn connection)
      | None ->
        raise Pool_message.Error.(Exn (DatabaseAddPoolFirst database_label))
    in
    run
  ;;

  let query database_label f =
    Caqti_lwt_unix.Pool.use (fun connection -> f connection)
    |> map_fetched database_label
    ||> raise_caqti_error
  ;;

  let collect label request input =
    query label (fun connection ->
      let module Connection = (val connection : Caqti_lwt.CONNECTION) in
      Connection.collect_list request input)
  ;;

  let exec label request input =
    query label (fun connection ->
      let module Connection = (val connection : Caqti_lwt.CONNECTION) in
      Connection.exec request input)
  ;;

  let find_opt label request input =
    query label (fun connection ->
      let module Connection = (val connection : Caqti_lwt.CONNECTION) in
      Connection.find_opt request input)
  ;;

  let find label request input =
    query label (fun connection ->
      let module Connection = (val connection : Caqti_lwt.CONNECTION) in
      Connection.find request input)
  ;;

  let populate label table columns request input =
    query label (fun connection ->
      let module Connection = (val connection : Caqti_lwt.CONNECTION) in
      Connection.populate
        ~table
        ~columns
        request
        (Caqti_lwt.Stream.of_list input)
      |> Lwt.map Caqti_error.uncongested)
  ;;

  let exec_each connection =
    Lwt_list.map_s (fun request -> request connection)
    %> Lwt.map CCResult.flatten_l
    %> Lwt_result.map Utils.flat_unit
  ;;

  let rollback connection error =
    let (module Connection : Caqti_lwt.CONNECTION) = connection in
    let%lwt () =
      Connection.rollback ()
      >|+ CCFun.tap (fun _ ->
        Logs.debug (fun m -> m "Successfully rolled back transaction"))
      ||> raise_caqti_error
    in
    Lwt.fail error
  ;;

  let transaction
    ?(setup : (Caqti_lwt.connection -> (unit, Caqti_error.t) Lwt_result.t) list =
      [])
    ?(cleanup :
        (Caqti_lwt.connection -> (unit, Caqti_error.t) Lwt_result.t) list =
      [])
    label
    (f : Caqti_lwt.connection -> ('a, Caqti_error.t) Lwt_result.t)
    : 'a Lwt.t
    =
    Caqti_lwt_unix.Pool.use (fun connection ->
      let (module Connection : Caqti_lwt.CONNECTION) = connection in
      let* () = Connection.start () in
      Lwt.catch
        (fun () ->
          let* () = exec_each connection setup in
          let* result = f connection in
          let* () = exec_each connection cleanup in
          match%lwt Connection.commit () with
          | Ok () -> Lwt.return_ok result
          | Error error -> Lwt.return_error error)
        (rollback connection))
    |> map_fetched label
    ||> raise_caqti_error
  ;;

  let transaction_iter label queries =
    Caqti_lwt_unix.Pool.use (fun connection ->
      let (module Connection : Caqti_lwt.CONNECTION) = connection in
      let* () = Connection.start () in
      Lwt.catch
        (fun () ->
          let* () = exec_each connection queries in
          Connection.commit ())
        (rollback connection))
    |> map_fetched label
    ||> raise_caqti_error
  ;;
end
