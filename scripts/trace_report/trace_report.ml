(* Usage: trace_report.exe [--top N] [--stacks SITE] DIR

   Prints a markdown summary of a run directory written by scripts/trace.sh:

   - per memtrace file (.ctf): estimated allocations per site, the first frame
     in the project's own code (pool/...), what is still live at the end of the
     trace (leak suspects) and the live heap over time. Sample counts are scaled
     by the sampling rate, so the byte numbers are estimates.
   - sanitizer logs (tsan.PID, asan.PID, lsan.PID): findings deduplicated by
     their SUMMARY line, leaks grouped by allocation stack. Logs of processes
     that wrote no memtrace file are the toolchain (dune, ocamlc, ... are built
     with the sanitizer too) and only listed. *)

open Memtrace.Trace

type site =
  { mutable samples : int
  ; mutable live : int
  }

type stats =
  { by_alloc : (string, site) Hashtbl.t
  ; by_pool : (string, site) Hashtbl.t
  ; live_objs : (string * string * int) Obj_id.Tbl.t
  ; mutable total : int
  ; mutable live_total : int
  ; mutable timeline : (int64 * int) list
  }

let site tbl key =
  match Hashtbl.find_opt tbl key with
  | Some site -> site
  | None ->
    let site = { samples = 0; live = 0 } in
    Hashtbl.add tbl key site;
    site
;;

let location_string { Location.filename; line; defname; _ } =
  Printf.sprintf "%s:%d %s" filename line defname
;;

let is_project { Location.filename; _ } = CCString.prefix ~pre:"pool/" filename

(* backtraces run from caller to callee, the allocation is the last frame *)
let frames reader buffer length =
  Array.sub buffer 0 length
  |> Array.to_list
  |> CCList.flat_map (Reader.lookup_location_code reader)
  |> List.rev
;;

let analyse reader =
  let duration = ref 0L in
  let stats =
    { by_alloc = Hashtbl.create 1024
    ; by_pool = Hashtbl.create 1024
    ; live_objs = Obj_id.Tbl.create 4096
    ; total = 0
    ; live_total = 0
    ; timeline = []
    }
  in
  let next_tick = ref 0L in
  let truncated = ref None in
  (try
     Reader.iter reader (fun time event ->
       let time = Timedelta.to_int64 time in
       duration := time;
       if Int64.compare time !next_tick >= 0
       then (
         stats.timeline <- (time, stats.live_total) :: stats.timeline;
         (* one checkpoint per second *)
         next_tick := Int64.add time 1_000_000L);
       match event with
       | Event.Alloc { obj_id; nsamples; backtrace_buffer; backtrace_length; _ } ->
         let frames = frames reader backtrace_buffer backtrace_length in
         let alloc =
           CCList.head_opt frames |> CCOption.map_or ~default:"(unknown)" location_string
         in
         (* innermost project frames with their callers, Lwt callbacks cut the
            stack at the scheduler *)
         let pool =
           match CCList.filter is_project frames with
           | [] -> "(outside pool/)"
           | project ->
             CCList.take 3 project |> List.map location_string |> String.concat " < "
         in
         List.iter
           (fun site ->
              site.samples <- site.samples + nsamples;
              site.live <- site.live + nsamples)
           [ site stats.by_alloc alloc; site stats.by_pool pool ];
         stats.total <- stats.total + nsamples;
         stats.live_total <- stats.live_total + nsamples;
         Obj_id.Tbl.replace stats.live_objs obj_id (alloc, pool, nsamples)
       | Event.Promote _ -> ()
       | Event.Collect obj_id ->
         (match Obj_id.Tbl.find_opt stats.live_objs obj_id with
          | None -> ()
          | Some (alloc, pool, nsamples) ->
            Obj_id.Tbl.remove stats.live_objs obj_id;
            List.iter
              (fun site -> site.live <- site.live - nsamples)
              [ site stats.by_alloc alloc; site stats.by_pool pool ];
            stats.live_total <- stats.live_total - nsamples))
   with
   (* NOTE: Timestamps are wall clock, the reader stops at the first one going
      backwards (e.g. the Docker Desktop VM resyncing its clock) *)
   | exn -> truncated := Some (Printexc.to_string exn));
  stats.timeline <- (!duration, stats.live_total) :: stats.timeline;
  stats, !duration, !truncated
;;

let bytes { Info.sample_rate; word_size; _ } samples =
  float_of_int samples /. sample_rate *. float_of_int (word_size / 8)
;;

let human bytes =
  let units = [ "B"; "KiB"; "MiB"; "GiB"; "TiB" ] in
  let rec go value = function
    | ([ unit ] | unit :: _) when value < 1024. -> Printf.sprintf "%.1f %s" value unit
    | _ :: units -> go (value /. 1024.) units
    | [] -> assert false
  in
  go bytes units
;;

let print_table info ~top ~title ~column ~value tbl =
  Printf.printf "\n#### %s\n\n| %s | site |\n|---:|---|\n" title column;
  Hashtbl.to_seq tbl
  |> List.of_seq
  |> List.filter (fun (_, site) -> value site > 0)
  |> List.sort (fun (_, a) (_, b) -> compare (value b) (value a))
  |> CCList.take top
  |> List.iter (fun (key, site) ->
    Printf.printf "| %s | `%s` |\n" (human (bytes info (value site))) key)
;;

let memtrace_report ~top filename =
  let reader = Reader.open_ ~filename in
  let info = Reader.info reader in
  let stats, duration, truncated = analyse reader in
  Reader.close reader;
  let seconds = Int64.to_float duration /. 1e6 in
  Printf.printf
    "\n\
     ### `%s`\n\n\
     - context: %s, pid %Ld, sampling rate %g, %d samples over %.1fs\n\
     - allocated: ~%s (~%s/s)\n\
     - still live at end of trace: ~%s\n"
    (Filename.basename filename)
    (CCOption.get_or ~default:"-" info.Info.context)
    info.Info.pid
    info.Info.sample_rate
    stats.total
    seconds
    (human (bytes info stats.total))
    (human (bytes info stats.total /. Float.max seconds 1.))
    (human (bytes info stats.live_total));
  truncated |> Option.iter (Printf.printf "- **truncated** after %.1fs: %s\n" seconds);
  if stats.total > 0
  then (
    print_table
      info
      ~top
      ~title:"Top allocation sites"
      ~column:"allocated"
      ~value:(fun s -> s.samples)
      stats.by_alloc;
    print_table
      info
      ~top
      ~title:"Top allocating project call paths (innermost first)"
      ~column:"allocated"
      ~value:(fun s -> s.samples)
      stats.by_pool;
    print_table
      info
      ~top
      ~title:"Still live at end of trace (leak suspects)"
      ~column:"live"
      ~value:(fun s -> s.live)
      stats.by_pool;
    let timeline = List.rev stats.timeline in
    let step = max 1 (List.length timeline / 10) in
    Printf.printf "\n#### Live heap over time\n\n| time | live |\n|---:|---:|\n";
    List.iteri
      (fun i (time, live) ->
         if i mod step = 0 || i = List.length timeline - 1
         then
           Printf.printf
             "| %.1fs | %s |\n"
             (Int64.to_float time /. 1e6)
             (human (bytes info live)))
      timeline);
  info.Info.pid
;;

type finding =
  { mutable reports : int
  ; mutable leaked : int
  ; mutable objects : int
  }

let leak_header line =
  try
    Scanf.sscanf line " %s leak of %d byte(s) in %d object(s)" (fun kind b o ->
      Some (kind, b, o))
  with
  | Scanf.Scan_failure _ | End_of_file | Failure _ -> None
;;

(* "    #3 0xb4de24 in caml_stat_strdup runtime/memory.c:730" *)
let frame line =
  match CCString.find ~sub:" in " line with
  | i when CCString.prefix ~pre:"#" (String.trim line) && i >= 0 ->
    Some (CCString.drop (i + 4) line)
  | _ -> None
;;

(* the allocator interceptor (frame #0) is the same for every leak *)
let leak_stack frames = CCList.drop 1 frames |> CCList.take 4 |> String.concat " < "

let parse_log findings leaks lines =
  let add tbl key ~leaked ~objects =
    let finding =
      match Hashtbl.find_opt tbl key with
      | Some finding -> finding
      | None ->
        let finding = { reports = 0; leaked = 0; objects = 0 } in
        Hashtbl.add tbl key finding;
        finding
    in
    finding.reports <- finding.reports + 1;
    finding.leaked <- finding.leaked + leaked;
    finding.objects <- finding.objects + objects
  in
  let rec go = function
    | [] -> ()
    | line :: rest ->
      (match leak_header line with
       | Some (kind, leaked, objects) ->
         let stack, rest = CCList.take_drop_while (fun l -> String.trim l <> "") rest in
         let key = kind ^ ": " ^ leak_stack (CCList.filter_map frame stack) in
         add leaks key ~leaked ~objects;
         go rest
       | None ->
         (match CCString.chop_prefix ~pre:"SUMMARY: " line with
          | Some summary when not (CCString.mem ~sub:"byte(s) leaked" summary) ->
            add findings summary ~leaked:0 ~objects:0
          | Some _ -> ()
          | None when CCString.mem ~sub:"has encountered a fatal error" line ->
            (* e.g. LSan's tracer crashing on the worker, the leak check didn't run *)
            add findings (line ^ " (check incomplete)") ~leaked:0 ~objects:0
          | None -> ());
         go rest)
  in
  go lines
;;

let sanitizer_log filename =
  match String.split_on_char '.' filename with
  | [ ("tsan" | "asan" | "lsan"); pid ] -> Int64.of_string_opt pid
  | _ -> None
;;

let sanitizer_report ~top ~pids dir files =
  let logs =
    CCList.filter_map (fun f -> sanitizer_log f |> CCOption.map (CCPair.make f)) files
  in
  if logs <> []
  then (
    let app, toolchain = List.partition (fun (_, pid) -> List.mem pid pids) logs in
    let findings = Hashtbl.create 64 in
    let leaks = Hashtbl.create 64 in
    List.iter
      (fun (file, _) ->
         CCIO.with_in (Filename.concat dir file) CCIO.read_lines_l
         |> parse_log findings leaks)
      app;
    let sorted tbl value =
      Hashtbl.to_seq tbl
      |> List.of_seq
      |> List.sort (fun (_, a) (_, b) -> compare (value b) (value a))
      |> CCList.take top
    in
    Printf.printf
      "\n\
       ## Sanitizer\n\n\
       - logs of traced processes: %s\n\
       - toolchain logs (not analysed): %s\n"
      (if app = [] then "none" else String.concat ", " (List.map fst app))
      (if toolchain = [] then "none" else String.concat ", " (List.map fst toolchain));
    Printf.printf "\n### Findings\n\n";
    if Hashtbl.length findings = 0
    then print_string "none\n"
    else (
      print_string "| reports | summary |\n|---:|---|\n";
      sorted findings (fun f -> f.reports)
      |> List.iter (fun (key, f) -> Printf.printf "| %d | `%s` |\n" f.reports key));
    (* NOTE: LSan doesn't scan OCaml 5's mmap'd major heap, so C memory that
       is only referenced by a live OCaml value (bigarrays, mutexes, SSL
       contexts, ...) is reported as a direct leak too. Indirect leaks are
       only reachable from direct ones and add nothing but noise. *)
    let direct, indirect =
      Hashtbl.fold (fun key f acc -> (key, f) :: acc) leaks []
      |> List.partition (fun (key, _) -> CCString.prefix ~pre:"Direct" key)
    in
    let sum = List.fold_left (fun acc (_, f) -> acc + f.leaked) 0 in
    Printf.printf
      "\n\
       ### Leaks\n\n\
       Direct leaks include C memory still owned by live OCaml values (LSan doesn't scan \
       the OCaml heap), so only leaks growing with the workload matter. Indirect leaks \
       (only reachable from direct ones): %s in %d stacks.\n\n"
      (human (float_of_int (sum indirect)))
      (List.length indirect);
    if direct = []
    then print_string "no direct leaks\n"
    else (
      print_string
        "| leaked | objects | reports | allocated from (innermost first) |\n\
         |---:|---:|---:|---|\n";
      List.sort (fun (_, a) (_, b) -> compare b.leaked a.leaked) direct
      |> CCList.take top
      |> List.iter (fun (key, f) ->
        Printf.printf
          "| %s | %d | %d | `%s` |\n"
          (human (float_of_int f.leaked))
          f.objects
          f.reports
          key)))
;;

(* --stacks SITE: the most allocating full stacks (innermost first) of the
   allocation sites containing SITE, to see who calls a hot library function *)
let stacks_report ~top ~site filename =
  let reader = Reader.open_ ~filename in
  let info = Reader.info reader in
  let stacks = Hashtbl.create 64 in
  (try
     Reader.iter reader (fun _ -> function
       | Event.Alloc { nsamples; backtrace_buffer; backtrace_length; _ } ->
         let frames = frames reader backtrace_buffer backtrace_length in
         (match frames with
          | alloc :: _ when CCString.mem ~sub:site (location_string alloc) ->
            let key = CCList.take 12 frames |> List.map location_string in
            Hashtbl.replace
              stacks
              key
              (nsamples + CCOption.get_or ~default:0 (Hashtbl.find_opt stacks key))
          | _ -> ())
       | Event.Promote _ | Event.Collect _ -> ())
   with
   | _ -> ());
  Reader.close reader;
  Printf.printf "\n### Stacks of `%s` in `%s`\n" site (Filename.basename filename);
  Hashtbl.to_seq stacks
  |> List.of_seq
  |> List.sort (fun (_, a) (_, b) -> compare b a)
  |> CCList.take top
  |> List.iter (fun (stack, samples) ->
    Printf.printf "\n%s:\n" (human (bytes info samples));
    List.iter (Printf.printf "    %s\n") stack)
;;

let () =
  let rec parse ((top, site, dir) as acc) = function
    | "--top" :: n :: rest -> parse (int_of_string n, site, dir) rest
    | "--stacks" :: s :: rest -> parse (top, Some s, dir) rest
    | dir :: rest -> parse (top, site, Some dir) rest
    | [] -> acc
  in
  match parse (15, None, None) (List.tl (Array.to_list Sys.argv)) with
  | _, _, None -> failwith "usage: trace_report.exe [--top N] [--stacks SITE] DIR"
  | top, site, Some dir ->
    let files = Sys.readdir dir |> Array.to_list |> List.sort compare in
    let traces = List.filter (CCString.suffix ~suf:".ctf") files in
    (match site with
     | Some site ->
       List.iter (fun f -> stacks_report ~top ~site (Filename.concat dir f)) traces
     | None ->
       if traces <> [] then print_string "\n## Memory (memtrace)\n";
       let pids =
         CCList.filter_map
           (fun file ->
              try Some (memtrace_report ~top (Filename.concat dir file)) with
              | exn ->
                Printf.printf
                  "\n### `%s`\n\nunreadable: %s\n"
                  file
                  (Printexc.to_string exn);
                None)
           traces
       in
       sanitizer_report ~top ~pids dir files)
;;
