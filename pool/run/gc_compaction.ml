(* OCaml 5 never compacts the major heap on its own: memory freed after a burst
   (e.g. a large mailing) stays mapped and the RSS keeps its peak. [Gc.compact]
   moves the live blocks together and hands the free pools back to the OS. It
   stops the world, but only takes a few milliseconds for heaps of this size. *)
let label = "gc compaction"
let src = Logs.Src.create "gc_compaction"
let tags = Database.(Logger.Tags.create Pool.Root.label)
let interval = Ptime.Span.of_int_s (60 * 60)
let mb words = words * (Sys.word_size / 8) / 1024 / 1024

let compact () =
  let before = Gc.quick_stat () in
  let started = Unix.gettimeofday () in
  let () = Gc.compact () in
  let duration = (Unix.gettimeofday () -. started) *. 1000. in
  let after = Gc.quick_stat () in
  Logs.info ~src (fun m ->
    m
      ~tags
      "Compacted heap in %.0fms: %dMB -> %dMB (peak %dMB)"
      duration
      (mb before.Gc.heap_words)
      (mb after.Gc.heap_words)
      (mb after.Gc.top_heap_words));
  Lwt.return_unit
;;

let start () =
  let open Schedule in
  create label (Every (interval |> ScheduledTimeSpan.of_span)) None compact
  |> add_and_start
;;

let lifecycle =
  Sihl.Container.create_lifecycle
    label
    ~dependencies:(fun () -> [ Schedule.lifecycle ])
    ~start
;;

let register () = Sihl.Container.Service.create lifecycle
