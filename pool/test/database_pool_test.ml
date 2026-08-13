(** Regression tests for the pool cache in [Database.Pool].

    These need no reachable database - they point a pool at a socket that
    behaves like a database that just went away. *)

let concurrent_queries = 5
let settle_timeout = 30.0

(** A socket that accepts connections and closes them again after a short,
    staggered delay, so the MariaDB handshake of one connection fails while the
    others are still in flight. *)
let start_unresponsive_server () =
  let socket = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Lwt_unix.setsockopt socket Unix.SO_REUSEADDR true;
  let%lwt () = Lwt_unix.bind socket (Unix.ADDR_INET (Unix.inet_addr_loopback, 0)) in
  let () = Lwt_unix.listen socket (concurrent_queries * 2) in
  let port =
    match Lwt_unix.getsockname socket with
    | Unix.ADDR_INET (_, port) -> port
    | Unix.ADDR_UNIX _ -> failwith "expected an inet socket"
  in
  let accepted = ref 0 in
  let () =
    Lwt.async (fun () ->
      let rec accept_loop () =
        let%lwt client, _ = Lwt_unix.accept socket in
        let delay = 0.5 +. (0.2 *. float_of_int !accepted) in
        incr accepted;
        Lwt.async (fun () ->
          let%lwt () = Lwt_unix.sleep delay in
          Lwt_unix.close client);
        accept_loop ()
      in
      (* Closing the socket ends the loop with EBADF; that is how it stops. *)
      Lwt.catch accept_loop (fun (_ : exn) -> Lwt.return_unit))
  in
  Lwt.return (port, fun () -> Lwt_unix.close socket)
;;

(** Every failing query disconnects its pool, and disconnecting drains it.
    Caqti's [Pool.drain] wakes a single waiter once the pool runs empty, so two
    drains of the same pool leave one of them - and the query behind it -
    pending forever, neither returning nor raising.

    That wedged the schedule loops: they all write their bookkeeping to the root
    database, so after an outage [pool_schedules] was never updated again and
    every service showed up as "not running" while it was running fine. *)
let unreachable_database_settles_every_query _ () =
  let open Database in
  let get_exn = Pool_message.Error.get_or_failwith in
  let%lwt port, close_server = start_unresponsive_server () in
  let label = Label.create "database-pool-test" |> get_exn in
  let url =
    Format.asprintf "mariadb://root@127.0.0.1:%d/test" port |> Url.create |> get_exn
  in
  let (_ : Label.t) = Pool.Tenant.add (create label url) in
  let request =
    let open Caqti_request.Infix in
    "SELECT 1" |> Caqti_type.(unit ->! int)
  in
  let query () =
    Lwt.catch
      (fun () -> find label request () |> Lwt.map (fun (_ : int) -> ()))
      (fun (_ : exn) -> Lwt.return_unit)
  in
  let queries = CCList.init concurrent_queries (fun (_ : int) -> query ()) in
  let%lwt settled =
    Lwt.pick
      [ Lwt.join queries |> Lwt.map (fun () -> true)
      ; Lwt_unix.sleep settle_timeout |> Lwt.map (fun () -> false)
      ]
  in
  let pending =
    CCList.count (fun query -> Lwt.state query = Lwt.Sleep) queries |> CCInt.to_string
  in
  let%lwt () = Pool.Tenant.drop label in
  let%lwt () = close_server () in
  Alcotest.(check string "no query is left pending" "0" pending);
  Alcotest.(check bool "every failing query settles" true settled) |> Lwt.return
;;
