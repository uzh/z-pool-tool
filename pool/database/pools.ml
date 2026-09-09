open CCFun
open Entity

exception Exception of string

let src = Logs.Src.create "pools"

module LogTag = struct
  let add_label : Label.t Logs.Tag.def =
    Logs.Tag.def "database_label" ~doc:"Database Label" Label.pp
  ;;

  let create database = Logs.Tag.(empty |> add add_label database)
end

module Make (Config : Pools_sig.ConfigSig) = struct
  module Config = Config

  type connection =
    | Close
    | Open of (Caqti_lwt.connection, Caqti_error.t) Caqti_lwt_unix.Pool.t
    | Fail of Caqti_error.t

  let fail err = Fail err

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
      { database; required; connection = Close; n_retries = 0 }
    ;;

    let reset_retry pool = { pool with n_retries = 0 }
    let increment_retry pool = { pool with n_retries = pool.n_retries + 1 }

    let connect_pool =
      let pool_config = Caqti_pool_config.create ~max_size:Config.database_pool_size () in
      (* Ptime values are written as UTC wall-clock; pin the session so NOW()
         and CURRENT_TIMESTAMP defaults agree with them regardless of the
         server's time_zone setting.

         Note: caqti-driver-mariadb has itself pinned every connection to UTC
         since its beginnings (Q.set_utc, "SET time_zone = '+00:00'"), so
         database sessions were always UTC and all stored values (DATETIME
         wall-clock and TIMESTAMP epochs) have always been UTC-consistent.
         This explicit pin only guards against the driver changing that behaviour.
         TIMESTAMP columns render in the inspecting session's time zone. *)
      let set_utc_request =
        let open Caqti_request.Infix in
        "SET time_zone = '+00:00'" |> Caqti_type.(unit ->. unit)
      in
      let post_connect (module Connection : Caqti_lwt.CONNECTION) =
        Connection.exec set_utc_request ()
      in
      Url.to_uri %> Caqti_lwt_unix.connect_pool ~pool_config ~post_connect
    ;;

    let connect_base ?(retries = 2) ({ required; _ } as pool) =
      let tags = pool |> database_label |> LogTag.create in
      CCResult.retry retries (fun () -> pool |> database_url |> connect_pool)
      |> (function
       | Error [] ->
         raise Pool_message.Error.(Exn (Unsupported "Failed to connect: empty error"))
       | Error (err :: _) when required -> raise (Caqti_error.Exn err)
       | Error (err :: _ as errors) ->
         Logs.warn ~src (fun m ->
           m ~tags "Failed to connect: %s" ([%show: Caqti_error.t list] errors));
         Fail err
       | Ok con -> Open con)
      |> fun connection -> { pool with connection }
    ;;

    module Cache = struct
      module Hashtbl = CCHashtbl.Make (Label)

      let pools : t Hashtbl.t = Hashtbl.create (max 1 Config.expected_databases)
      let clear () = Hashtbl.clear pools
      let add = Hashtbl.add pools
      let remove = Hashtbl.remove pools
      let find_opt = Hashtbl.find_opt pools
      let replace pool = Hashtbl.replace pools (database_label pool) pool

      let log_pools ?src ?(level = Logs.Debug) () =
        Logs.msg ?src level (fun m ->
          m "%s" ([%show: t list] (Hashtbl.values_list pools)))
      ;;

      let all
            ?(allowed_status = Status.all)
            ?(exclude : Label.t list = [ Entity.root ])
            ()
        =
        Hashtbl.to_list pools
        |> CCList.filter_map (fun (label, { database; _ }) ->
          (CCList.exists (Label.equal label) exclude |> not
           && CCList.exists (Status.equal database.status) allowed_status)
          |> CCBool.if_then (fun () -> database))
      ;;

      let find_by_status ?(exclude : Label.t list = [ Entity.root ]) status =
        Hashtbl.values_list pools
        |> CCList.filter_map (fun { database; _ } ->
          let with_status_and_not_excluded =
            CCList.exists (Status.equal database.status) status
            && not (CCList.exists (Label.equal database.label) exclude)
          in
          CCBool.if_then (fun () -> database) with_status_and_not_excluded)
      ;;

      let find_by_url ?(allowed_status = Status.[ Active ]) url =
        Hashtbl.values_list pools
        |> CCList.find_opt (fun { database; _ } ->
          Url.equal database.url url
          && CCList.exists (Status.equal database.status) allowed_status)
        |> CCOption.map (fun { database; _ } -> database)
        |> CCOption.to_result Pool_message.(Error.NotFound Field.Url)
      ;;
    end

    let find =
      Cache.find_opt
      %> CCOption.map (fun { database; _ } -> database)
      %> CCOption.to_result Pool_message.(Error.NotFound Field.Label)
    ;;

    let find_all = Cache.all
    let find_by_status = Cache.find_by_status
    let find_by_url = Cache.find_by_url
    let clear = Cache.clear

    let print_usage ?tags =
      connection
      %> function
      | Open pool ->
        let n_connections = Caqti_lwt_unix.Pool.size pool in
        Logs.debug ~src (fun m ->
          m ?tags "Pool usage: %i/%i" n_connections Config.database_pool_size)
      | Close | Fail _ ->
        Logs.debug ~src (fun m -> m ?tags "Pool usage: No connection found")
    ;;

    let drain_opt pool =
      Lwt.dont_wait
        (fun () ->
           connection pool
           |> function
           | Open pool -> Caqti_lwt_unix.Pool.drain pool
           | Close | Fail _ -> Lwt.return_unit)
        (fun exn ->
           Logs.warn ~src (fun m ->
             m
               "Draining pool '%s' failed: %s"
               (database_label pool)
               (Printexc.to_string exn)))
    ;;

    let add ?required database =
      let label = database |> label in
      match Cache.find_opt label with
      | Some _ ->
        let msg =
          [%string "Failed to add pool: Pool already exists %{Label.value label}"]
        in
        Logs.err ~src (fun m -> m ~tags:(label |> LogTag.create) "%s" msg);
        failwith msg
      | None -> create ?required database |> Cache.add label
    ;;

    let drop name =
      match Cache.find_opt name with
      | None ->
        let msg =
          [%string
            "Failed to drop pool: connection to '%{Label.value name}' doesn't exist"]
        in
        Logs.info ~src (fun m -> m ~tags:(LogTag.create name) "%s" msg)
      | Some pool ->
        let () = Cache.remove name in
        drain_opt pool
    ;;

    let initialize ?(additional_pools : Entity.t list = []) () : unit =
      Config.database :: additional_pools
      |> CCList.filter (label %> Cache.find_opt %> CCOption.is_none)
      |> CCList.iter (create ~required:true %> Cache.replace)
    ;;

    let connect =
      Cache.find_opt
      %> function
      | Some pool ->
        let rec connect pool =
          match pool.connection with
          | Fail err -> Error (Pool_message.Error.CaqtiError (Caqti_error.show err))
          | Close -> connect_base pool |> connect
          | Open _ -> Ok ()
        in
        connect pool
      | None -> Error Pool_message.(Error.NotFound Field.Database)
    ;;

    let disconnect' ?error ?note =
      Cache.find_opt
      %> function
      | Some pool ->
        let default =
          CCOption.map_or
            (function
              | `Info msg | `Warning msg | `Error msg -> msg)
            ~default:"Unknown error"
            note
          |> Format.asprintf " (%s)"
        in
        let message =
          CCOption.map_or
            ~default
            (Format.asprintf " with error: %a" Caqti_error.pp)
            error
        in
        let level =
          match note with
          | Some (`Info _) -> Logs.Info
          | Some (`Warning _) -> Logs.Warning
          | Some (`Error _) | None -> Logs.Error
        in
        Logs.msg ~src level (fun m ->
          m "Disconnect pool '%s'%s" (database_label pool) message);
        let () =
          Cache.replace
            { pool with connection = CCOption.map_or ~default:Close fail error }
        in
        drain_opt pool
      | None -> ()
    ;;

    let disconnect ?error = disconnect' ?error ?note:None

    let reset ?required database =
      let () = disconnect' ~note:(`Info "DB reset") (label database) in
      create ?required database |> Cache.replace
    ;;

    let raise_caqti_error_labelled (label : Entity.Label.t) input =
      let open Caqti_error in
      match%lwt input with
      | Ok resp -> Lwt.return resp
      | Error `Unsupported ->
        let () = disconnect' ~note:(`Error "Unsupported") label in
        raise Pool_message.Error.(Exn (Unsupported "Caqti error"))
      | Error (#t as err) ->
        let () = disconnect' ~error:err label in
        let () = Cache.log_pools () in
        raise (Exn err)
    ;;

    let rec fetch ?(retries = 2) label =
      let () = Cache.log_pools ~level:Logs.Debug () in
      match Cache.find_opt label with
      | Some ({ n_retries; _ } as pool) ->
        (match connection pool with
         | Fail err when n_retries >= retries ->
           (* Give up for this call, but reset the retry counter so subsequent
              fetches attempt to reconnect again. Otherwise the pool stays in
              [Fail] forever and never recovers once the database is back. *)
           let () = reset_retry pool |> Cache.replace in
           raise_caqti_error_labelled (database_label pool) (Error err |> Lwt_result.lift)
         | Fail _ ->
           let () = connect_base pool |> increment_retry |> Cache.replace in
           fetch ~retries label
         | Close ->
           let () = connect_base pool |> Cache.replace in
           fetch ~retries label
         | Open connection when n_retries > 0 ->
           let () = reset_retry pool |> Cache.replace in
           print_usage pool;
           Lwt.return connection
         | Open connection ->
           print_usage pool;
           Lwt.return connection)
      | None -> raise Pool_message.Error.(Exn (DatabaseAddPoolFirst label))
    ;;

    let disconnect_and_raise_on_error connection label m =
      match%lwt m with
      | Ok x -> Lwt.return x
      | Error error ->
        let module Connection = (val connection : Caqti_lwt.CONNECTION) in
        let%lwt () = Connection.disconnect () in
        Lwt.fail (Database_error.Failed (Database_error.create label error))

    let map_fetched (type maybe_txn) ?retries (ctx : maybe_txn Entity.ctx) (fcn : 'a -> ('b, 'e) Lwt_result.t) =
      match ctx with
      | Label { label; tags = _ } ->
        let%lwt connection = fetch ?retries label in
        let fcn connection =
          fcn connection
          |> disconnect_and_raise_on_error connection label
          |> Lwt_result.ok
        in
        let%lwt r = Caqti_lwt_unix.Pool.use fcn connection in
        (* [get_ok r] is safe because only [fcn] above can return the [Error _] case *)
        Lwt.return (Result.get_ok r)
      | Connection { connection; label; tags = _ } | TransactionalConnection{ connection; label; tags = _ } ->
        fcn connection
        |> disconnect_and_raise_on_error connection label


    let raise_caqti_error (type maybe_transaction) (ctx : maybe_transaction Entity.ctx) input =
      let label = match ctx with
        | Label { label; _ } | Connection { label; _ } | TransactionalConnection { label; _ } -> label
      in
      raise_caqti_error_labelled label input
  end

  let query db_ctx f =
    Pool.map_fetched db_ctx f
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
      Connection.populate ~table ~columns request (Caqti_lwt.Stream.of_list input)
      |> Lwt.map Caqti_error.uncongested)
  ;;

  let in_transaction_sql =
    let open Caqti_request.Infix in
    {sql|select @@in_transaction|sql}
    |> Caqti_type.unit ->! Caqti_type.bool

  let exec_each fns connection =
    let open Utils.Lwt_result.Infix in
    List.fold_left
      (fun acc fn ->
         acc >>= fun () -> fn connection)
      (Lwt_result.return ())
      fns
  ;;

  let transaction db_ctx ?(setup=[]) ?(cleanup=[]) fn =
    query db_ctx @@ fun ((module Connection : Caqti_lwt.CONNECTION) as connection) ->
    let open Utils.Lwt_result.Infix in
    let fn' () =
      exec_each setup connection >>= fun () ->
      fn connection >>= fun result ->
      exec_each cleanup connection >>= fun () ->
      Lwt_result.return result
    in
    Connection.find in_transaction_sql () >>= fun in_transaction ->
    if in_transaction then
      fn' ()
    else
      Connection.with_transaction fn'

  let transaction_iter db_ctx ?setup ?cleanup fs =
    transaction db_ctx ?setup ?cleanup (exec_each fs)

  let label_ctx ?tags label =
    let tags = Logger.Tags.extend label tags in
    Entity.Label { label; tags }

  let connection_ctx ?tags label fcn =
    let tags = Logger.Tags.extend label tags in
    let%lwt pool = Pool.fetch label in
    Caqti_lwt_unix.Pool.use
      (fun connection ->
         let ctx = Entity.Connection { connection; label; tags } in
         fcn ctx
         |> Lwt_result.ok)
      pool
    |> Lwt.map Result.get_ok
  (* XXX(reynir): This is safe because we always return [Ok _] or raise an
     exception . The type of [Caqti_lwt_unix.Pool.use] forces us to return a [_
     result Lwt.t], but the type also tells us that it doesn't return errors
     other than what [fcn] returns. *)

  let transaction_ctx ?tags label fcn =
    let tags = Logger.Tags.extend label tags in
    let%lwt pool = Pool.fetch label in
    Caqti_lwt_unix.Pool.use
      (fun connection ->
         let open Lwt_result.Syntax in
         let (module Connection : Caqti_lwt.CONNECTION) = connection in
         let ctx = Entity.TransactionalConnection { connection; label; tags } in
         Pool.raise_caqti_error ctx @@
         let* () = Connection.start () in
         Lwt.catch (fun () ->
             let%lwt result = fcn ctx in
             let* () = Connection.commit () in
             Lwt.return_ok result)
           (fun exn ->
              let%lwt () =
                Pool.raise_caqti_error ctx @@
                let+ () = Connection.rollback () in
                Logs.debug (fun m -> m "Successfully rolled back transaction")
              in
              Lwt.reraise exn)
         |> Lwt_result.ok)
      pool
    |> Lwt.map Result.get_ok
    (* XXX(reynir): This is safe because we always return [Ok _] or raise an
       exception . The type of [Caqti_lwt_unix.Pool.use] forces us to return a [_
       result Lwt.t], but the type also tells us that it doesn't return errors
       other than what [fcn] returns. *)

end
