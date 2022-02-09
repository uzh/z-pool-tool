let reporter =
  let token = Sys.getenv_opt "CI_JOB_TOKEN" in
  let project_id = Sys.getenv_opt "CI_PROJECT_ID" in
  let project_name = Sys.getenv_opt "CI_PROJECT_NAME" in
  let uri_base = Sys.getenv_opt "CI_API_V4_URL" in
  let has_env_vars =
    not (List.exists (( = ) None) [ token; project_id; project_name; uri_base ])
  in
  if has_env_vars
  then (
    let module Gitlab_notify =
      Notify_failures.Notifier.Gitlab (struct
        let token = Option.get token
        let uri_base = Option.get uri_base
        let project_name = Option.get project_name
        let project_id = int_of_string (Option.get project_id)
      end)
    in
    fun req exc backtrace ->
      let additional =
        let buffer = Buffer.create 4_096 in
        let formatter = Format.formatter_of_buffer buffer in
        Format.pp_print_string formatter "```\n";
        Opium.Request.pp_hum formatter req;
        Format.pp_print_string formatter "\n```";
        Format.pp_print_flush formatter ();
        Buffer.contents buffer
      in
      let%lwt res =
        Gitlab_notify.make_gitlab_notifier ~additional exc backtrace
      in
      match res with
      | Ok iid ->
        Logs.info (fun m -> m "Successfully reported error to gitlab.");
        Lwt.return_ok iid
      | Error err ->
        Logs.info (fun m -> m "Unable to report error to gitlab: %s" err);
        Lwt.return_error err)
  else
    fun _req _exc _backtrace ->
    Logs.err (fun m ->
        m "Unable to get environment variables to report error to gitlab.");
    Lwt.return_ok (-1)
;;

let error () =
  Sihl.Web.Middleware.error
    ~reporter:(fun req exc ->
      let%lwt (_ : (int, string) result) =
        reporter req exc (Printexc.get_backtrace ())
      in
      Lwt.return_unit)
    ()
;;
