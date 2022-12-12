type config =
  { token : string
  ; uri_base : string
  ; project_id : int
  ; project_name : string
  }

let config token uri_base project_id project_name =
  { token; uri_base; project_id; project_name }
;;

let schema =
  Conformist.(
    make
      Field.
        [ string
            ~meta:
              "Used by canary (Exception Notifier) to create an issue in GitLab"
            "GITLAB_TOKEN"
        ; string
            ~meta:
              "Used by canary (Exception Notifier) to create an issue in GitLab"
            "GITLAB_API_BASE"
        ; int
            ~meta:
              "Used by canary (Exception Notifier) to create an issue in GitLab"
            "GITLAB_PROJECT_ID"
        ; string
            ~meta:
              "Used by canary (Exception Notifier) to create an issue in GitLab"
            "GITLAB_PROJECT_NAME"
        ]
      config)
;;

let before_start () =
  if Sihl.Configuration.is_production ()
  then (
    (* Validate configuration variables for production environment*)
    let _ = Sihl.Configuration.(read schema) in
    ())
  else ()
;;

let reporter =
  let config = Sihl.Configuration.(read schema) in
  let module Gitlab_notify =
    Canary.Notifier.Gitlab (struct
      let token = config.token
      let uri_base = config.uri_base
      let project_name = config.project_name
      let project_id = config.project_id
    end)
  in
  fun exc ->
    let { Sihl.Web.Middleware.exn; req_id; req; _ } = exc in
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
    let%lwt res =
      Gitlab_notify.notify ~additional (Failure exn) (Printexc.get_backtrace ())
    in
    match res with
    | Ok iid ->
      Logs.info (fun m ->
        m "Successfully reported error to gitlab as issue %d." iid);
      Lwt.return_ok iid
    | Error err ->
      Logs.info (fun m -> m "Unable to report error to gitlab: %s" err);
      Lwt.return_error err
;;

let error () =
  Sihl.Web.Middleware.error
    ~reporter:(fun _ exn ->
      match%lwt reporter exn with
      | Ok _ -> Lwt.return_unit
      | Error err -> raise (Failure err))
    ()
;;
