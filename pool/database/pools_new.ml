(* Log under the "database" source, like the rest of this library. *)
module Log = Service.Logs

let or_raise label m =
  match%lwt m with
  | Ok x -> Lwt.return x
  | Error error -> Lwt.fail (Database_error.Failed (Database_error.create label error))
;;

let use label f =
  let%lwt pool = Service.Pool.fetch label in
  Caqti_lwt_unix.Pool.use (fun connection -> Lwt.map CCResult.return (f connection)) pool
  |> or_raise label
;;

(* caqti-driver-mariadb 2.3.0 clears autocommit in [start] and restores it in
   [commit] — but not in [rollback]. A rolled-back connection therefore goes back
   to the pool with an implicit transaction open, and the next borrower's writes
   are never committed. Restoring via [commit] would work but reads as a commit;
   issue the SET explicitly instead. It is only safe after a *successful*
   rollback, since otherwise it would commit the partial work.

   This is the one place outside [Database_new.Request] that names
   [Caqti_request] directly. *)
let restore_autocommit_request =
  let open Caqti_request.Infix in
  "SET autocommit = 1" |> Caqti_type.(unit ->. unit) ~oneshot:true
;;

let rollback ?tags label (module C : Caqti_lwt.CONNECTION) =
  let tags = Logger.Tags.extend label tags in
  let complain step error =
    Log.err (fun m -> m ~tags "%s failed: %s" step (Caqti_error.show error))
  in
  match%lwt C.rollback () with
  | Error error -> Lwt.return (complain "rollback" error)
  | Ok () ->
    C.exec restore_autocommit_request ()
    |> Lwt.map (function
      | Ok () -> ()
      | Error error -> complain "restoring autocommit" error)
;;
