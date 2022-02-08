module Gitlab_notify = Notify_failures.Notifier.Gitlab (struct
  let token = "gegHG2SwvZPxsY-TLUoJ"
  let uri_base = "https://gitlab.uzh.ch/api/v4"
  let project_name = "pool-tool"
  let project_id = 2700
end)

let reporter req exc backtrace =
  let additional =
    let buffer = Buffer.create 4_096 in
    let formatter = Format.formatter_of_buffer buffer in
    Format.pp_print_string formatter "```\n";
    Opium.Request.pp_hum formatter req;
    Format.pp_print_string formatter "\n```";
    Format.pp_print_flush formatter ();
    Buffer.contents buffer
  in
  let%lwt res = Gitlab_notify.make_gitlab_notifier ~additional exc backtrace in
  match res with
  | Ok iid ->
    Logs.info (fun m -> m "Successfully reported error to gitlab.");
    Lwt.return_ok iid
  | Error err ->
    Logs.info (fun m -> m "Unable to report error to gitlab: %s" err);
    Lwt.return_error err
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
