let src = Logs.Src.create "middleware.error"

let reporter
  (request : Rock.Request.t)
  { Sihl.Web.Middleware.exn; req_id; req; _ }
  =
  let open Format in
  let formatter = str_formatter in
  let additional =
    let error =
      pp_print_string formatter (asprintf "\n\nException:\n");
      pp_print_text formatter (asprintf "\n```\n%s\n```\n" exn);
      flush_str_formatter ()
    in
    let trace =
      asprintf "\nTrace:\n```\n%s\n```\n" (Printexc.get_backtrace ())
    in
    let request =
      pp_print_string formatter (asprintf "\nRequest: %s\n" req_id);
      pp_print_string formatter (asprintf "\n```\n%s\n```\n" req);
      flush_str_formatter ()
    in
    asprintf "%s\n\n%s\n\n%s\n" error trace request
  in
  Pool_canary.notify
    ~src
    ~tags:(Pool_context.Logger.Tags.req request)
    ~labels:[ "Bug"; "exception" ]
    ~additional
    (Failure exn)
    (Printexc.get_backtrace ())
;;

let error () =
  Sihl.Web.Middleware.error
    ~reporter:(fun req exn ->
      match%lwt reporter req exn with
      | Ok _ -> Lwt.return_unit
      | Error err -> failwith err)
    ()
;;
