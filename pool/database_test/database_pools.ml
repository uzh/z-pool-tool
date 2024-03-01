open CCFun.Infix

exception Exception of string

module type Sig = Database_pools_sig.Sig

let src = Logs.Src.create "test.pools"
let find_pool_name = CCList.assoc_opt ~eq:CCString.equal "pool"

module LogTag = struct
  let add_label : string Logs.Tag.def =
    Logs.Tag.def "database_label" ~doc:"Database Label" CCString.pp
  ;;

  let create database = Logs.Tag.(empty |> add add_label database)

  let ctx_opt ?ctx () =
    let open CCOption.Infix in
    ctx >>= find_pool_name >|= fun db -> Logs.Tag.(empty |> add add_label db)
  ;;
end

type connection_type =
  | SinglePool of string
  | MultiPools of (string * string * bool) list

let with_log ?tags ?(log_level = Logs.Error) ?(msg_prefix = "Error") err =
  let msg = Caqti_error.show err in
  Logs.msg ~src log_level (fun m -> m ?tags "%s: %s" msg_prefix msg);
  msg
;;

let get_or_raise ?ctx ?tags ?log_level ?msg_prefix () =
  let tags = CCOption.or_ ~else_:(LogTag.ctx_opt ?ctx ()) tags in
  function
  | Ok result -> result
  | Error error -> failwith (with_log ?tags ?log_level ?msg_prefix error)
;;

module type ConfigSig = sig
  val database : connection_type
  val database_pool_size : int
end

module DefaultConfig : ConfigSig = struct
  let database = SinglePool "mariadb://root@database:3306/test"
  let database_pool_size = 5
end

module Make (Config : ConfigSig) = struct
  type status =
    ( (Caqti_lwt.connection, Caqti_error.t) Caqti_lwt_unix.Pool.t
      , Caqti_error.load )
      result

  let main_pool_ref
    : (Caqti_lwt.connection, Caqti_error.t) Caqti_lwt_unix.Pool.t option ref
    =
    ref None
  ;;

  let pools : (string, status) Hashtbl.t =
    let spare_for_pools = 5 in
    Hashtbl.create
      (match Config.database with
       | SinglePool _ -> 1
       | MultiPools pools -> CCList.length pools + spare_for_pools)
  ;;

  let print_pool_usage ?tags pool =
    let n_connections = Caqti_lwt_unix.Pool.size pool in
    let max_connections = Config.database_pool_size in
    Logs.debug ~src (fun m ->
      m ?tags "Pool usage: %i/%i" n_connections max_connections)
  ;;

  let connect
    ?(retries = 10)
    ?(pool_size = Config.database_pool_size)
    ?(required = false)
    store_fcn
    name
    database_url
    =
    let tags = LogTag.create name in
    let connect () =
      database_url
      |> Uri.of_string
      |> Caqti_lwt_unix.connect_pool
           ~pool_config:(Caqti_pool_config.create ~max_size:pool_size ())
    in
    CCResult.retry retries connect
    |> function
    | Ok con -> Ok con |> CCFun.tap store_fcn
    | Error (err :: _) when required -> failwith (with_log ~tags err)
    | Error (err :: _) -> Error err |> CCFun.tap store_fcn
    | Error [] -> failwith "Failed to connect: empty error"
  ;;

  let add_pool ?required ?pool_size name database_url =
    match Config.database, Hashtbl.find_opt pools name with
    | SinglePool _, _ ->
      failwith "SinglePool is selected: Switch to 'MultiPools' first"
    | MultiPools _, Some _ ->
      let msg =
        [%string
          "Failed to create pool: Connection pool with name '%{name}' exists \
           already"]
      in
      Logs.err ~src (fun m -> m ~tags:(LogTag.create name) "%s" msg);
      failwith msg
    | MultiPools _, None ->
      connect ?required ?pool_size (Hashtbl.add pools name) name database_url
  ;;

  let drop_pool name =
    let remove = Hashtbl.remove pools in
    match Config.database, Hashtbl.find_opt pools name with
    | SinglePool _, _ ->
      failwith "SinglePool is selected: Switch to 'MultiPools' first"
    | MultiPools _, None ->
      let msg =
        [%string "Failed to drop pool: connection to '%{name}' doesn't exist"]
      in
      Logs.warn ~src (fun m -> m ~tags:(LogTag.create name) "%s" msg);
      Lwt.return_unit
    | MultiPools _, Some (Ok connection) ->
      let () = remove name in
      Caqti_lwt_unix.Pool.drain connection
    | MultiPools _, Some (Error _) -> remove name |> Lwt.return
  ;;

  let initialize () : unit =
    let show name = function
      | Ok _ -> ()
      | Error _ ->
        Logs.warn ~src (fun m ->
          m
            ~tags:(LogTag.create name)
            "%s"
            [%string "Failed to initialize pool '%{name}"]);
        ()
    in
    match Config.database with
    | SinglePool database_url when CCOption.is_none !main_pool_ref ->
      let name = "main" in
      let store = function
        | Ok pool ->
          main_pool_ref := Some pool;
          ()
        | Error _ ->
          failwith
            "Missing database connection: required for SinglePool configuration"
      in
      connect ~required:true store name database_url |> show name
    | SinglePool _ -> ()
    | MultiPools pools' ->
      pools'
      |> CCList.filter (fun (name, _, _) ->
        CCOption.is_none (Hashtbl.find_opt pools name))
      |> CCList.iter (fun (name, url, required) ->
        connect ~required (Hashtbl.add pools name) name url |> show name)
  ;;

  let fetch_pool ?(ctx = []) () =
    let open CCOption in
    let () = initialize () in
    match Config.database with
    | SinglePool _ ->
      !main_pool_ref
      |> get_exn_or "Initialization missed: run 'initialize'"
      |> CCResult.return
    | MultiPools _ ->
      let test : (string * status option) option =
        find_pool_name ctx >|= fun name -> name, Hashtbl.find_opt pools name
      in
      test
      |> (function
       | Some (_, Some (Ok status)) -> Ok status
       | Some (name, Some (Error err)) ->
         connect
           (CCFun.tap (Hashtbl.replace pools name))
           name
           (err |> Caqti_error.uri |> Uri.to_string)
       | Some (_, None) | None ->
         failwith "Unknown Pool: Please 'add_pool' first!")
  ;;

  let map_fetched ?ctx (fcn : 'a -> ('b, 'e) Lwt_result.t)
    : ('b, 'e) Lwt_result.t
    =
    let open Caqti_error in
    match fetch_pool ?ctx () with
    | Error (#load as resp) -> Lwt.return_error resp
    | Ok pool ->
      print_pool_usage pool;
      fcn pool
  ;;

  let query ?ctx f =
    Caqti_lwt_unix.Pool.use (fun connection -> f connection) |> map_fetched ?ctx
  ;;

  let collect ?ctx request input =
    query ?ctx (fun connection ->
      let module Connection = (val connection : Caqti_lwt.CONNECTION) in
      Connection.collect_list request input)
  ;;

  let exec ?ctx request input =
    query ?ctx (fun connection ->
      let module Connection = (val connection : Caqti_lwt.CONNECTION) in
      Connection.exec request input)
  ;;

  let find_opt ?ctx request input =
    query ?ctx (fun connection ->
      let module Connection = (val connection : Caqti_lwt.CONNECTION) in
      Connection.find_opt request input)
  ;;

  let find ?ctx request input =
    query ?ctx (fun connection ->
      let module Connection = (val connection : Caqti_lwt.CONNECTION) in
      Connection.find request input)
  ;;

  let populate ?ctx table columns request input =
    query ?ctx (fun connection ->
      let module Connection = (val connection : Caqti_lwt.CONNECTION) in
      Connection.populate
        ~table
        ~columns
        request
        (Caqti_lwt.Stream.of_list input)
      |> Lwt.map Caqti_error.uncongested)
  ;;

  let transaction
    ?ctx
    (f : Caqti_lwt.connection -> ('a, Caqti_error.t) Lwt_result.t)
    : ('a, Caqti_error.t) Lwt_result.t
    =
    let open Lwt_result.Infix in
    Caqti_lwt_unix.Pool.use (fun connection ->
      let (module Connection : Caqti_lwt.CONNECTION) = connection in
      let rollback err =
        Connection.rollback ()
        |> CCFun.flip Lwt.bind (function
          | Ok () ->
            let (_ : string) =
              let tags = LogTag.ctx_opt ?ctx () in
              let log_level = Logs.Debug in
              with_log ?tags ~log_level ~msg_prefix:"Rollback Successful" err
            in
            Lwt.return_error err
          | Error err -> Lwt.return_error err)
      in
      Connection.start ()
      >>= Connection.commit
      >>= fun () ->
      f connection
      |> CCFun.flip Lwt.bind (function
        | Ok res -> Lwt.return_ok res
        | Error err -> rollback err))
    |> map_fetched ?ctx
  ;;

  let transaction_exn ?ctx = transaction ?ctx %> Lwt.map (get_or_raise ?ctx ())
end
