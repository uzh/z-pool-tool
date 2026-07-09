(* Extracted from the Sihl web framework (https://github.com/oxidizing/sihl), MIT license.
   Source:
   https://github.com/uzh/sihl/blob/6c3c4040413294155cda3a363edf0ff3c7e638b8/sihl/src/core_service.ml *)

module type Sig = sig
  val lifecycle : Lifecycle.lifecycle
end

type t =
  { lifecycle : Lifecycle.lifecycle
  ; configuration : Configuration.t
  ; commands : Command.t list
  ; server : bool
  }
[@@deriving eq, show]

let commands (service : t) = service.commands
let configuration service = service.configuration

let create
      ?(commands = [])
      ?(configuration = Configuration.empty)
      ?(server = false)
      lifecycle
  =
  { lifecycle; configuration; commands; server }
;;

let server t = t.server
let start t = t.lifecycle.start ()
let stop t = t.lifecycle.stop ()
let id t = t.lifecycle.id
let name t = Lifecycle.human_name t.lifecycle

let start_services services =
  Logs.info (fun m -> m "Starting...");
  let lifecycles = List.map (fun service -> service.lifecycle) services in
  let lifecycles = lifecycles |> Lifecycle.top_sort_lifecycles in
  let%lwt () =
    Lwt_list.iter_s
      (fun (lifecycle : Lifecycle.lifecycle) ->
         Logs.debug (fun m -> m "Starting service: %s" @@ Lifecycle.human_name lifecycle);
         lifecycle.Lifecycle.start ())
      lifecycles
  in
  Logs.info (fun m -> m "All services started.");
  Lwt.return lifecycles
;;

let stop_services services =
  Logs.info (fun m -> m "Stopping...");
  let lifecycles = List.map (fun service -> service.lifecycle) services in
  let lifecycles = lifecycles |> Lifecycle.top_sort_lifecycles in
  let%lwt () =
    Lwt_list.iter_s
      (fun (lifecycle : Lifecycle.lifecycle) ->
         Logs.debug (fun m -> m "Stopping service: %s" @@ Lifecycle.human_name lifecycle);
         lifecycle.Lifecycle.stop ())
      lifecycles
  in
  Logs.info (fun m -> m "Stopped, Good Bye!");
  Lwt.return ()
;;
