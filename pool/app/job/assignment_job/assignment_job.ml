include Handler

module Job = struct
  open CCFun

  let handle ?id:_ pool _ =
    let open Utils.Lwt_result.Infix in
    Lwt.catch
      (fun () -> update_upcoming_assignments pool ||> CCResult.return)
      (Printexc.to_string %> Pool_message.Error.unsupported %> Lwt.return_error)
  ;;

  let job =
    let open CCFun in
    let open Database in
    let encode = Label.yojson_of_t %> Yojson.Safe.to_string in
    let decode = Yojson.Safe.from_string %> Label.t_of_yojson %> CCResult.return in
    Pool_queue.Job.create
      handle
      ~max_tries:10
      ~retry_delay:(Sihl.Time.Span.hours 1)
      encode
      decode
      Pool_queue.JobName.CheckMatchesFilter
  ;;

  let dispatch pool =
    Logs.debug ~src (fun m ->
      m
        ~tags:(Database.Logger.Tags.create pool)
        "Dispatch 'update assignments' of %s"
        (Database.Label.value pool));
    Pool_queue.dispatch pool pool job
  ;;
end

let run_all () =
  Database.(Pool.Tenant.all ()) |> Lwt_list.iter_s update_upcoming_assignments
;;

let start () =
  let open Schedule in
  let interval = Ptime.Span.of_int_s (60 * 60) |> ScheduledTimeSpan.of_span in
  let periodic_fcn () =
    Logs.debug ~src (fun m -> m ~tags:Database.(Logger.Tags.create Pool.Root.label) "Run");
    run_all ()
  in
  create "upcoming_assignments_match_filter" (Every interval) None periodic_fcn
  |> Schedule.add_and_start
;;

let lifecycle =
  Sihl.Container.create_lifecycle
    "System events"
    ~dependencies:(fun () -> [ Pool_database.lifecycle; Pool_queue.lifecycle_service ])
    ~start
;;

let register () = Sihl.Container.Service.create lifecycle
let job = Job.job
let dispatch_update_upcomming = Job.dispatch

type event = Dispatched [@@deriving eq, show]

let handle_event pool : event -> unit Lwt.t = function
  | Dispatched -> Job.dispatch pool
;;
