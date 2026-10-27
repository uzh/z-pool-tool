(* NOTE: Lwt_unix wakes the event loop for its handlers, a plain [Sys.set_signal]
   handler only runs when the loop wakes up by itself (up to minutes in the worker) *)
let exit_on_signal signal = Lwt_unix.on_signal signal (fun _ -> exit 0) |> ignore

let rec mkdir_p dir =
  if not (Sys.file_exists dir)
  then (
    mkdir_p (Filename.dirname dir);
    try Sys.mkdir dir 0o755 with
    | Sys_error _ when Sys.file_exists dir -> ())
;;

(* Sihl sets ROOT_PATH (the repository) while loading the env file, test
   executables run in the build directory *)
let resolve filename =
  match Sys.getenv_opt "ROOT_PATH" with
  | Some root when root <> "" && Filename.is_relative filename ->
    Filename.concat root filename
  | Some _ | None -> filename
;;

let start ~context =
  match Sys.getenv_opt "MEMTRACE" with
  | None | Some "" -> ()
  | Some filename ->
    let pid = Unix.getpid () |> string_of_int in
    let filename = CCString.replace ~sub:"%p" ~by:pid filename |> resolve in
    Unix.putenv "MEMTRACE" filename;
    (try
       mkdir_p (Filename.dirname filename);
       Memtrace.trace_if_requested ~context ();
       prerr_endline ("memtrace: writing " ^ filename);
       exit_on_signal Sys.sigint;
       exit_on_signal Sys.sigterm
     with
     | Invalid_argument msg | Sys_error msg -> prerr_endline ("memtrace disabled: " ^ msg))
;;
