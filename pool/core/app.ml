(* Extracted from the Sihl web framework (https://github.com/oxidizing/sihl), MIT license.
   Source:
   https://github.com/uzh/sihl/blob/6c3c4040413294155cda3a363edf0ff3c7e638b8/sihl/src/core_app.ml
   The unused code generator commands ([Gen.commands]) have been removed. *)

let log_src = Logs.Src.create "pool.core.app"

module App_logs = (val Logs.src_log log_src : Logs.LOG)

exception Exception of string

type t =
  { services : Container.Service.t list
  ; before_start : unit -> unit Lwt.t
  ; after_stop : unit -> unit Lwt.t
  }

let empty =
  { services = []
  ; before_start = (fun _ -> Lwt.return ())
  ; after_stop = (fun _ -> Lwt.return ())
  }
;;

let with_services services app = { app with services }
let before_start before_start app = { app with before_start }
let after_stop after_stop app = { app with after_stop }

let run_forever () =
  let p, _ = Lwt.wait () in
  p
;;

let start_cmd services =
  Command.make
    ~name:"server"
    ~description:"Starts the app including all registered services and the HTTP server."
    (fun _ ->
       let normal_services =
         List.filter (fun service -> not (Container.Service.server service)) services
       in
       let server_services = List.filter Container.Service.server services in
       match server_services with
       | [ server ] ->
         let%lwt _ = Container.start_services normal_services in
         let%lwt () = Container.Service.start server in
         run_forever ()
       | [] ->
         App_logs.err (fun m ->
           m
             "No 'server' service registered. Make sure that you have one server service \
              registered in your 'run.ml' such as a HTTP service");
         raise (Exception "No server service registered")
       | servers ->
         let names = List.map Container.Service.name servers in
         let names = String.concat ", " names in
         App_logs.err (fun m ->
           m
             "Multiple server services registered: '%s', you can only have one service \
              registered that is a 'server' service."
             names);
         raise (Exception "Multiple server services registered"))
;;

let run' ?(commands = []) ?(log_reporter = Log.default_reporter) ?args app =
  (* Set the logger up as first thing so we can log *)
  Logs.set_reporter log_reporter;
  App_logs.info (fun m -> m "Setting up...");
  App_logs.debug (fun m -> m "Setup configurations");
  let configurations = List.map Container.Service.configuration app.services in
  let () = Configuration.load () in
  let%lwt () = app.before_start () in
  let configuration_commands = Configuration.commands configurations in
  App_logs.debug (fun m -> m "Setup service commands");
  let service_commands =
    app.services |> List.map Container.Service.commands |> List.concat
  in
  let start_server_cmd = start_cmd app.services in
  let commands =
    List.concat
      [ [ start_server_cmd ]
      ; [ Random.random_cmd ]
      ; configuration_commands
      ; service_commands
      ; commands
      ]
  in
  (* Make sure that the secret is valid *)
  let _ = Configuration.read_secret () in
  Command.run commands args
;;

let run ?(commands = []) ?(log_reporter = Log.default_reporter) ?args app =
  Lwt_main.run
  @@
  match args with
  | Some args -> run' ~commands ~log_reporter ~args app
  | None -> run' ~commands ~log_reporter app
;;
