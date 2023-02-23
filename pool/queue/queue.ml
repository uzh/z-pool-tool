include Sihl.Contract.Queue

let log_src = Logs.Src.create "pool.queue"

module Logs = (val Logs.src_log log_src : Logs.LOG)

let increment_tries (retry_delay : Ptime.Span.t) (job_instance : instance) =
  let next_run_at =
    match Ptime.add_span job_instance.next_run_at retry_delay with
    | Some date -> date
    | None -> failwith "Can not determine next run date of job"
  in
  { job_instance with tries = job_instance.tries + 1; next_run_at }
;;

let registered_jobs : job' list ref = ref []
let stop_schedule : (unit -> unit) option ref = ref None

let run_job
  (input : string)
  ({ handle; failed; _ } : job')
  ({ id; ctx; _ } as job_instance : instance)
  : (unit, string) Lwt_result.t
  =
  let with_log_error message exn =
    let exn_string = Printexc.to_string exn in
    Logs.err (fun m -> m message exn_string);
    Lwt.return_error exn_string
  in
  let ctx = if CCList.is_empty ctx then None else Some ctx in
  let%lwt result =
    Lwt.catch
      (fun () -> handle ?ctx input)
      (with_log_error
         "Exception caught while running job, this is a bug in your job \
          handler. Don't throw exceptions there, use Result.t instead. '%s'")
  in
  match result with
  | Error msg ->
    Logs.err (fun m ->
      m
        "Failure while running job instance %s %s"
        ([%show: instance] job_instance)
        msg);
    Lwt.catch
      (fun () ->
        let%lwt () = failed ?ctx msg job_instance in
        Lwt.return_error msg)
      (with_log_error
         "Exception caught while cleaning up job, this is a bug in your job \
          failure handler, make sure to not throw exceptions there '%s")
  | Ok () ->
    Logs.debug (fun m -> m "Successfully ran job instance '%s'" id);
    Lwt.return @@ Ok ()
;;

let update ?ctx job_instance = Repo.update ?ctx job_instance

let work_job
  ({ retry_delay; max_tries; _ } as job : job')
  ({ input; tries; ctx; _ } as job_instance : instance)
  =
  let ctx = if CCList.is_empty ctx then None else Some ctx in
  let now = Ptime_clock.now () in
  if should_run job_instance now
  then (
    let%lwt job_run_status = run_job input job job_instance in
    let job_instance = job_instance |> increment_tries retry_delay in
    let job_instance =
      match job_run_status with
      | Error msg when tries >= max_tries ->
        { job_instance with
          status = Failed
        ; last_error = Some msg
        ; last_error_at = Some (Ptime_clock.now ())
        }
      | Error msg ->
        { job_instance with
          last_error = Some msg
        ; last_error_at = Some (Ptime_clock.now ())
        }
      | Ok () -> { job_instance with status = Succeeded }
    in
    update ?ctx job_instance)
  else (
    Logs.debug (fun m ->
      m "Not going to run job instance %s" ([%show: instance] job_instance));
    Lwt.return_unit)
;;

let work_queue database_labels jobs =
  let open Utils.Lwt_result.Infix in
  let%lwt pending_job_instances =
    Lwt_list.map_s
      (fun label -> Repo.find_workable ~ctx:(Pool_tenant.to_ctx label) ())
      database_labels
    ||> CCList.flatten
  in
  if CCList.is_empty pending_job_instances
  then Lwt.return_unit
  else (
    Logs.info (fun m ->
      m "Start working queue of length %d" (CCList.length pending_job_instances));
    let rec loop job_instances jobs =
      match job_instances with
      | [] -> Lwt.return_unit
      | (job_instance : instance) :: job_instances ->
        CCList.find_opt
          (fun (job : job') -> job.name |> String.equal job_instance.name)
          jobs
        |> (function
        | None -> loop job_instances jobs
        | Some job -> work_job job job_instance)
    in
    let%lwt () = loop pending_job_instances jobs in
    Logs.info (fun m -> m "Finish working queue");
    Lwt.return_unit)
;;

let start () =
  let open Utils.Lwt_result.Infix in
  let open Sihl.Schedule in
  Logs.debug (fun m -> m "Start job queue");
  (* This function runs every second, the request context gets created here with
     each tick *)
  let periodic_function () =
    let%lwt database_labels =
      Pool_tenant.find_all ()
      ||> CCList.map (fun { Pool_tenant.database_label; _ } -> database_label)
    in
    let database_labels = Pool_database.root :: database_labels in
    Logs.debug (fun m ->
      m
        "Running Queue for databases: %s"
        ([%show: Pool_database.Label.t list] database_labels));
    let jobs = !registered_jobs in
    if CCList.is_empty jobs
    then (
      Logs.debug (fun m -> m "No jobs found to run, trying again later");
      Lwt.return_unit)
    else (
      let job_strings = jobs |> CCList.map (fun (job : job') -> job.name) in
      Logs.debug (fun m ->
        m
          "Run job queue with registered jobs: %s"
          ([%show: string list] job_strings));
      work_queue database_labels jobs)
  in
  let job_queue = create every_second periodic_function "job_queue" in
  stop_schedule := Some (schedule job_queue);
  Lwt.return_unit
;;

let stop () =
  registered_jobs := [];
  (match !stop_schedule with
   | Some stop_schedule -> stop_schedule ()
   | None -> Logs.warn (fun m -> m "Can not stop schedule"));
  Lwt.return_unit
;;

let lifecycle =
  Sihl.Container.create_lifecycle
    "Multitenant Queue"
    ~dependencies:(fun () ->
      [ Sihl.Schedule.lifecycle
      ; Sihl.Database.lifecycle
      ; Pool_tenant.Service.Queue.lifecycle
      ])
    ~start
    ~stop
;;

let register ?(jobs = []) () =
  registered_jobs := !registered_jobs @ jobs;
  let configuration = Sihl.Configuration.make () in
  Sihl.Container.Service.create ~configuration lifecycle
;;
