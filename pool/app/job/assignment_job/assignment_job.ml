module Queue = Sihl_queue.MariaDb
include Handler

module Job = struct
  open CCFun

  let handle ?(ctx : (string * string) list option) pool =
    let open Utils.Lwt_result.Infix in
    let _ = ctx in
    Lwt.catch
      (fun () -> update_upcoming_assignments pool ||> CCResult.return)
      (Printexc.to_string %> Lwt.return_error)
  ;;

  let job =
    let open CCFun in
    let open Pool_database in
    let encode = Label.yojson_of_t %> Yojson.Safe.to_string in
    let decode =
      Yojson.Safe.from_string %> Label.t_of_yojson %> CCResult.return
    in
    Sihl.Contract.Queue.create_job
      handle
      ~max_tries:10
      ~retry_delay:(Sihl.Time.Span.hours 1)
      encode
      decode
      "check_matches_filter"
  ;;

  let dispatch pool =
    Logs.debug ~src (fun m ->
      m
        ~tags:(Pool_database.Logger.Tags.create pool)
        "Dispatch 'update assignments' of %s"
        (Pool_database.Label.value pool));
    Queue.dispatch ~ctx:(Pool_database.to_ctx pool) pool job
  ;;
end

let run_all () =
  let open Utils.Lwt_result.Infix in
  Pool_tenant.find_databases ()
  >|> Lwt_list.iter_s (fun { Pool_database.label; _ } ->
    update_upcoming_assignments label)
;;

let start () =
  let open Schedule in
  let interval = Ptime.Span.of_int_s (60 * 60) |> ScheduledTimeSpan.of_span in
  let periodic_fcn () =
    Logs.debug ~src (fun m ->
      m ~tags:Pool_database.(Logger.Tags.create root) "Run");
    run_all ()
  in
  create "upcoming_assignments_match_filter" (Every interval) periodic_fcn
  |> Schedule.add_and_start
;;

let stop () = Lwt.return_unit

let lifecycle =
  Sihl.Container.create_lifecycle
    "System events"
    ~dependencies:(fun () ->
      [ Database.lifecycle; Email.Service.Queue.lifecycle ])
    ~start
    ~stop
;;

let register () = Sihl.Container.Service.create lifecycle
let job = Job.job
let dispatch_update_upcomming = Job.dispatch

type event = Dispatched [@@deriving eq, show]

let handle_event pool : event -> unit Lwt.t = function
  | Dispatched -> Job.dispatch pool
;;
