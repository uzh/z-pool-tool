let src = Logs.Src.create "pool-healthcheck"

open Lwt.Infix

let check_server_health url =
  let open Cohttp_lwt_unix in
  Client.get (Uri.of_string url)
  >>= fun (resp, body) ->
  let%lwt () = Cohttp_lwt.Body.drain_body body in
  match Cohttp.Response.status resp with
  | `OK ->
    Logs.debug (fun m -> m "[Health Check] Server is healthy (%s)" url);
    Lwt.return_unit
  | _ ->
    Logs.err (fun m -> m "[Health Check] Server is unhealthy (%s)" url);
    exit 1
;;

(* Periodically check the Opium server's health *)
let rec monitor_server url interval =
  Lwt.catch
    (fun () -> check_server_health url)
    (fun exn ->
       Logs.warn (fun m ->
         m "[Health Check] Failed (%s): %s" url (Printexc.to_string exn));
       Lwt.return_unit)
  >>= fun () -> Lwt_unix.sleep interval >>= fun () -> monitor_server url interval
;;

let start ?tags () =
  Logs.info ~src (fun m -> m ?tags "Start schedule");
  let port = Sihl.Configuration.read_int "PORT" |> CCOption.value ~default:3000 in
  let url =
    (* TODO: setup for staging/production environment *)
    Format.asprintf "http://localhost:%d%s" port ("/health" |> Sihl.Web.externalize_path)
  in
  Lwt.async (fun () -> monitor_server url 5.0);
  Lwt.return_unit
;;

let lifecycle = Sihl.Container.create_lifecycle "pool healthcheck" ~start
let register () = Sihl.Container.Service.create lifecycle
