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

  let ctx_opt ?ctx () =
    let open CCOption.Infix in
    ctx
    >>= CCList.assoc_opt ~eq:CCString.equal "pool"
    >|= Label.of_string
    >|= fun db -> Logs.Tag.(empty |> add add_label db)
  ;;
end

let with_log ?tags ?(log_level = Logs.Error) ?(msg_prefix = "Error") err =
  let msg = Caqti_error.show err in
  Logs.msg ~src log_level (fun m -> m ?tags "%s: %s" msg_prefix msg);
  msg
;;

let raise_caqti_error ?tags =
  let open Caqti_error in
  function
  | Ok resp -> resp
  | Error `Unsupported ->
    Logs.err ~src (fun m -> m ?tags "Caqti error unsupported");
    failwith "Caqti error unsupported"
  | Error (#t as err) -> raise (Exn err)
;;

let get_or_raise ?ctx ?tags ?log_level ?msg_prefix () =
  let tags = CCOption.or_ ~else_:(LogTag.ctx_opt ?ctx ()) tags in
  function
  | Ok result -> result
  | Error error -> failwith (with_log ?tags ?log_level ?msg_prefix error)
;;

module DefaultConfig : Pools_sig.ConfigSig = struct
  let database =
    let open Entity in
    create
      (Label.of_string "main")
      (Url.of_string "mariadb://root@database:3306/test")
  ;;

  let database_pool_size = 5
  let expected_databases = 1
end

module Make (Config : Pools_sig.ConfigSig) = struct
  type status =
    ( (Caqti_lwt.connection, Caqti_error.t) Caqti_lwt_unix.Pool.t
      , Caqti_error.load )
      result

  module Config = Config
  module Hashtbl = CCHashtbl.Make (Label)

  let pools : status Hashtbl.t =
    Hashtbl.create (max 1 Config.expected_databases)
  ;;

  let print_pool_usage ?tags pool =
    let n_connections = Caqti_lwt_unix.Pool.size pool in
    let max_connections = Config.database_pool_size in
    Logs.debug ~src (fun m ->
      m ?tags "Pool usage: %i/%i" n_connections max_connections)
  ;;

  let connect
    ?(retries = 2)
    ?(pool_size = Config.database_pool_size)
    ?(required = false)
    store_fcn
    database
    =
    let tags = database |> label |> LogTag.create in
    let connect () =
      database
      |> url
      |> Url.to_uri
      |> Caqti_lwt_unix.connect_pool
           ~pool_config:(Caqti_pool_config.create ~max_size:pool_size ())
    in
    CCResult.retry retries connect
    |> function
    | Ok con -> Ok con |> CCFun.tap store_fcn
    | Error (err :: _) when required -> failwith (with_log ~tags err)
    | Error (err :: _) ->
      Logs.warn ~src (fun m -> m ~tags "Failed to connect: %a" pp database);
      Error err |> CCFun.tap store_fcn
    | Error [] -> failwith "Failed to connect: empty error"
  ;;

  let add_pool ?required ?pool_size database =
    let label = database |> label in
    match Hashtbl.find_opt pools label with
    | Some _ ->
      let msg =
        [%string
          "Failed to add pool: Connection already exists %{Label.show label}"]
      in
      Logs.err ~src (fun m -> m ~tags:(label |> LogTag.create) "%s" msg);
      failwith msg
    | None -> connect ?required ?pool_size (Hashtbl.add pools label) database
  ;;

  let drop_pool name =
    let remove = Hashtbl.remove pools in
    match Hashtbl.find_opt pools name with
    | None ->
      let msg =
        [%string
          "Failed to drop pool: connection to '%{Label.show name}' doesn't \
           exist"]
      in
      Logs.warn ~src (fun m -> m ~tags:(LogTag.create name) "%s" msg);
      Lwt.return_unit
    | Some (Ok connection) ->
      let () = remove name in
      Caqti_lwt_unix.Pool.drain connection
    | Some (Error _) -> remove name |> Lwt.return
  ;;

  let initialize ?(additinal_pools : Entity.t list = []) () : unit =
    let ignore = CCFun.const () in
    Config.database :: additinal_pools
    |> CCList.filter (label %> Hashtbl.find_opt pools %> CCOption.is_none)
    |> CCList.iter (fun database ->
      let store = database |> label |> Hashtbl.add pools in
      connect ~required:true store database |> ignore)
  ;;

  let fetch_pool database_label =
    Hashtbl.find_opt pools database_label
    |> function
    | Some (Ok status) -> Ok status
    | Some (Error err) ->
      let database =
        create
          database_label
          (err |> Caqti_error.uri |> Uri.to_string |> Url.of_string)
      in
      connect (CCFun.tap (Hashtbl.replace pools database_label)) database
    | None -> failwith "Unknown Pool: Please 'add_pool' first!"
  ;;

  let map_fetched database_label (fcn : 'a -> ('b, 'e) Lwt_result.t)
    : ('b, 'e) Lwt_result.t
    =
    let open Caqti_error in
    match fetch_pool database_label with
    | Error (#load as resp) -> Lwt.return_error resp
    | Ok pool ->
      print_pool_usage pool;
      fcn pool
  ;;

  let query database_label f =
    Caqti_lwt_unix.Pool.use (fun connection -> f connection)
    |> map_fetched database_label
    ||> raise_caqti_error ~tags:(LogTag.create database_label)
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
    match%lwt Connection.rollback () with
    | Ok () ->
      Logs.debug (fun m -> m "Successfully rolled back transaction");
      Lwt.fail error
    | Error error -> Lwt.return_error error
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
          let%lwt result = f connection in
          let* () = exec_each connection cleanup in
          let result = result |> CCResult.get_exn in
          match%lwt Connection.commit () with
          | Ok () -> Lwt.return_ok result
          | Error error -> Lwt.return_error error)
        (rollback connection))
    |> map_fetched label
    ||> raise_caqti_error ~tags:(LogTag.create label)
  ;;

  let transactions label queries =
    Caqti_lwt_unix.Pool.use (fun connection ->
      let (module Connection : Caqti_lwt.CONNECTION) = connection in
      let* () = Connection.start () in
      Lwt.catch
        (fun () ->
          let* () = exec_each connection queries in
          Connection.commit ())
        (rollback connection))
    |> map_fetched label
    ||> raise_caqti_error ~tags:(LogTag.create label)
  ;;
end
