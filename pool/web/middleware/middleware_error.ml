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

let reporter req =
  let config = Sihl.Configuration.(read schema) in
  let module Gitlab_notify =
    Canary.Notifier.Gitlab (struct
      let token = config.token
      let uri_base = config.uri_base
      let project_name = config.project_name
      let project_id = config.project_id
    end)
  in
  fun exc backtrace ->
    let additional =
      let buffer = Buffer.create 4_096 in
      let formatter = Format.formatter_of_buffer buffer in
      Format.pp_print_string formatter "```\n";
      Opium.Request.pp_hum formatter req;
      Format.pp_print_string formatter "\n```\n\nTrace:\n\n```\n";
      Format.pp_print_string formatter backtrace;
      Format.pp_print_string formatter "\n```";
      Format.pp_print_flush formatter ();
      Buffer.contents buffer
    in
    let%lwt res = Gitlab_notify.notify ~additional exc backtrace in
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
    ~reporter:(fun req exc ->
      match%lwt reporter req (Failure exc) (Printexc.get_backtrace ()) with
      | Ok _ -> Lwt.return_unit
      | Error err -> raise (Failure err))
    ()
;;
