(* This implementation is based on
   https://github.com/rgrinberg/opium/blob/master/opium/src/middlewares/middleware_debugger.ml
   but it removes the detailed error message to prevent leaking information. *)

let log_src = Logs.Src.create "middleware.error"

module Logs = (val Logs.src_log log_src : Logs.LOG)

module Report = struct
  type t =
    { exn : exn
    ; stack : string
    ; req_id : string
    ; req : string
    }

  let create exn req =
    let stack = Printexc.get_backtrace () in
    let req_id = Sihl.Web.Id.find req |> CCOption.value ~default:"-" in
    let req = Format.asprintf "%a" Opium.Request.pp_hum req in
    { exn; stack; req_id; req }
  ;;

  let to_string { exn; stack; req_id; req } =
    let exn_string = Printexc.to_string exn in
    [%string
      "Request id %{req_id}: %{req}\nError: %{exn_string}\nStacktrace: %{stack}"]
  ;;
end

let site_error_handler req =
  let request_id = Sihl.Web.Id.find req |> CCOption.value ~default:"-" in
  Page.Utils.error request_id |> Sihl.Web.Response.of_html |> Lwt.return
;;

let json_error_handler req =
  let request_id = Sihl.Web.Id.find req |> CCOption.value ~default:"-" in
  let msg = "Something went wrong, our administrators have been notified." in
  let body =
    [%string {|"{"errors": ["%{msg}"], "request_id": "%{request_id}"}"|}]
  in
  Opium.Response.of_plain_text body
  |> Opium.Response.set_content_type "application/json; charset=utf-8"
  |> Opium.Response.set_status `Internal_server_error
  |> Lwt.return
;;

let issue_reporter
  (request : Rock.Request.t)
  { Report.exn; req_id; req; stack; _ }
  =
  let open Format in
  let formatter = str_formatter in
  let additional =
    let error =
      pp_print_string formatter (asprintf "\n\nException:\n");
      pp_print_text
        formatter
        (asprintf "\n```\n%s\n```\n" (Printexc.to_string exn));
      flush_str_formatter ()
    in
    let trace = asprintf "\nTrace:\n```\n%s\n```\n" stack in
    let request =
      pp_print_string formatter (asprintf "\nRequest: %s\n" req_id);
      pp_print_string formatter (asprintf "\n```\n%s\n```\n" req);
      flush_str_formatter ()
    in
    asprintf "%s\n\n%s\n\n%s\n" error trace request
  in
  Pool_canary.notify
    ~src:log_src
    ~tags:(Pool_context.Logger.Tags.req request)
    ~labels:[ "Bug"; "exception" ]
    ~additional
    exn
    stack
;;

let reporter req ({ Report.exn; _ } as excn) =
  match[@warning "-4"] exn with
  | Caqti_error.(Exn #load_or_connect as err)
  | Pool_message.Error.(Exn DatabaseAddPoolFirst as err)
  | Pool_message.Error.(Exn (Connection _) as err) ->
    let%lwt () = System_event.Service.ConnectionWatcher.verify_tenants () in
    Logs.err (fun m -> m "Try again later: %s" (Printexc.to_string err))
    |> Lwt.return
  | _ ->
    (match%lwt issue_reporter req excn with
     | Ok _ -> Lwt.return_unit
     | Error err -> failwith err)
;;

let middleware ?error_handler () =
  let filter handler req =
    (* Make sure to Lwt.catch everything that might go wrong. *)
    Lwt.catch
      (fun () -> handler req)
      (fun exn ->
        let report = Report.create exn req in
        Logs.err (fun m -> m "%s" (Report.to_string report));
        let () =
          Lwt.dont_wait
            (fun () -> reporter req report)
            (fun exn ->
              let msg = Printexc.to_string exn in
              Logs.err (fun m ->
                m "Failed to run custom error reporter: %s" msg))
        in
        match error_handler with
        | Some error_handler -> error_handler req
        | None ->
          req
          |> Opium.Request.header "Content-Type"
          |> CCOption.map (CCString.split_on_char ';')
          |> CCFun.flip CCOption.bind CCList.head_opt
          |> (function
           | Some "application/json" -> json_error_handler req
           (* Default to text/html *)
           | Some _ | None -> site_error_handler req))
  in
  (* In a production setting we don't want to use the built in debugger
     middleware of opium. It is useful for development but it exposed too much
     information. *)
  if Sihl.Configuration.is_production ()
  then Rock.Middleware.create ~name:"error" ~filter
  else Opium.Middleware.debugger
;;
