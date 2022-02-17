let reporter =
  let envs =
    let getenv_result env =
      Sys.getenv_opt env
      |> CCOption.to_result
           (Format.asprintf "Couldn't find environment variable %s" env)
    in
    let open CCResult in
    let* token = getenv_result "CI_JOB_TOKEN" in
    let* project_id =
      getenv_result "CI_PROJECT_ID"
      >>= fun x ->
      CCInt.of_string x
      |> Option.to_result
           ~none:"Couldn't find environment variable CI_PROJECT_ID"
    in
    let* project_name = getenv_result "CI_PROJECT_NAME" in
    let* uri_base = getenv_result "CI_API_V4_URL" in
    Ok (token, project_id, project_name, uri_base)
  in
  match envs with
  | Ok (token, project_id, project_name, uri_base) ->
    let module Gitlab_notify =
      Notify_failures.Notifier.Gitlab (struct
        let token = token
        let uri_base = uri_base
        let project_name = project_name
        let project_id = project_id
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
      (match res with
      | Ok iid ->
        Logs.info (fun m -> m "Successfully reported error to gitlab.");
        Lwt.return_ok iid
      | Error err ->
        Logs.info (fun m -> m "Unable to report error to gitlab: %s" err);
        Lwt.return_error err)
  | Error msg ->
    fun _req _exc _backtrace ->
      Lwt.return_error
        (Format.asprintf
           {|Unable to get environment variable %s to report error to gitlab.|}
           msg)
;;

let error () =
  Sihl.Web.Middleware.error
    ~reporter:(fun req exc ->
      match%lwt reporter req exc (Printexc.get_backtrace ()) with
      | Ok _ -> Lwt.return_unit
      | Error err -> raise (Failure err))
    ()
;;
