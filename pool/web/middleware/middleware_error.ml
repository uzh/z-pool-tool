module Gitlab_notify = Notify_failures.Notifier.Gitlab (struct
  let token = "gegHG2SwvZPxsY-TLUoJ"
  let uri_base = "https://gitlab.uzh.ch/api/v4"
  let project_name = "pool-tool"
  let project_id = 2700
end)

let reporter req exc backtrace =
  let additional =
    let buffer = Buffer.create 4_096 in
    let () =
      let formatter = Format.formatter_of_buffer buffer in
      let () = Format.pp_print_string formatter "```\n" in
      let () = Opium.Request.pp_hum formatter req in
      let () = Format.pp_print_string formatter "\n```" in
      Format.pp_print_flush formatter ()
    in
    Buffer.contents buffer
  in
  let%lwt res = Gitlab_notify.make_gitlab_notifier ~additional exc backtrace in
  match res with
  | Ok iid ->
    let _ = Logs.info (fun m -> m "Successfully reported error to gitlab.") in
    Lwt.return_ok iid
  | Error err ->
    let _ = Logs.info (fun m -> m "Unable to report error to gitlab: %s" err) in
    Lwt.return_error err
;;

let sihl_middleware =
  Sihl.Web.Middleware.error
    ~reporter:(fun req exc ->
      let%lwt _ = reporter req exc (Printexc.get_backtrace ()) in
      Lwt.return_unit)
    ()
;;

let test_middleware =
  let filter handler req =
    Lwt.catch
      (fun () -> handler req)
      (fun exc ->
        let open Tyxml in
        let%lwt issue_link =
          match%lwt
            reporter req (Printexc.to_string exc) (Printexc.get_backtrace ())
          with
          | Ok iid ->
            let uri =
              Format.asprintf
                "https://gitlab.uzh.ch/econ/study-coordination/pool/-/issues/%d"
                iid
              |> Uri.of_string
            in
            let%html x =
              "<a href='"
                (Uri.to_string uri)
                "'>Click here to go to see the issue on GitLab</a>"
            in
            Lwt.return [ x ]
          | Error err ->
            let%html x =
              {|Error! Failed to create/update the GitLab issue for this \
               exception.
               <br>
                <pre>
                  <code>|}
                [ Html.txt err ]
                {|</code>
                </pre>|}
            in
            Lwt.return x
        in
        [%html
          {|
        <html>
          <head>
            <title>Unhandled Exception</title>
          </head>
          <body>
            <h1>Unhandled exception encountered at the top level.</h1>
            <h2>Generating (or updating) a GitLab issue. Click below to view issue:</h2>
            |}
            issue_link
            {|
          </body>
        </html>|}]
        |> Opium.Response.of_html
        |> Opium.Response.set_content_type "text/html;\n     charset=utf-8"
        |> Lwt.return)
  in
  Rock.Middleware.create ~name:"error" ~filter
;;

let error () =
  (* The Sihl error handling middleware includes its own `is_production` check.
     This prevents us from running our custom reporter when running the pool
     tool in test environments, which is particularly problematic when one is
     trying to test issue creation itself. *)
  if Sihl.Configuration.is_production ()
  then sihl_middleware
  else test_middleware
;;
