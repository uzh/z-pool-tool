open CCFun
include Entity
module Guard = Entity_guard

let src = Logs.Src.create "queue.service"
let tags = Database.(Logger.Tags.create root)

let increment_tries (retry_delay : Ptime.Span.t) (job_instance : instance) =
  let next_run_at =
    match Ptime.add_span job_instance.next_run_at retry_delay with
    | Some date -> date
    | None -> failwith "Can not determine next run date of job"
  in
  { job_instance with tries = job_instance.tries + 1; next_run_at }
;;

let registered_jobs : job' list ref = ref []
let find = Repo.find
let find_by = Repo.find_by
let count_workable = Repo.count_workable
let dispatch = Service.dispatch
let dispatch_all = Service.dispatch_all

let run_job
  ?tags
  (input : string)
  ({ handle; failed; _ } : job')
  ({ id; ctx; _ } as job_instance : instance)
  : (unit, string) Lwt_result.t
  =
  let with_log_error ?tags message exn =
    let exn_string = Printexc.to_string exn in
    Logs.err ~src (fun m -> m ?tags message exn_string);
    Lwt.return_error exn_string
  in
  let label = Database.of_ctx_exn ctx in
  let%lwt result =
    Lwt.catch
      (fun () -> handle label input)
      (with_log_error
         ?tags
         "Exception caught while running job, this is a bug in your job \
          handler. Don't throw exceptions there, use Result.t instead. '%s'")
  in
  match result with
  | Error msg ->
    Logs.err ~src (fun m ->
      m
        ?tags
        "Failure while running job instance %s %s"
        ([%show: instance] job_instance)
        msg);
    Lwt.catch
      (fun () ->
        let%lwt () = failed label msg job_instance in
        Lwt.return_error msg)
      (with_log_error
         ?tags
         "Exception caught while cleaning up job, this is a bug in your job \
          failure handler, make sure to not throw exceptions there '%s")
  | Ok () ->
    Logs.debug ~src (fun m -> m "Successfully ran job instance '%s'" id);
    Lwt.return @@ Ok ()
;;

let update = Repo.update

let work_job
  ({ retry_delay; max_tries; _ } as job : job')
  ({ input; tries; ctx; _ } as job_instance : instance)
  =
  let database_label = Database.of_ctx_exn ctx in
  let tags = Database.Logger.Tags.create database_label in
  let now = Ptime_clock.now () in
  if should_run job_instance now
  then (
    let%lwt job_run_status = run_job ~tags input job job_instance in
    let job_instance = job_instance |> increment_tries retry_delay in
    let job_instance =
      match job_run_status with
      | Error msg when tries >= max_tries ->
        { job_instance with
          status = Status.Failed
        ; last_error = Some msg
        ; last_error_at = Some (Ptime_clock.now ())
        }
      | Error msg ->
        { job_instance with
          last_error = Some msg
        ; last_error_at = Some (Ptime_clock.now ())
        }
      | Ok () -> { job_instance with status = Status.Succeeded }
    in
    let%lwt () = Notifier.job_reporter job_instance in
    update database_label job_instance)
  else (
    Logs.debug ~src (fun m ->
      m
        ~tags
        "Not going to run job instance %s"
        ([%show: instance] job_instance));
    Lwt.return_unit)
;;

let work_queue database_labels jobs =
  let open Utils.Lwt_result.Infix in
  let%lwt pending_job_instances =
    Lwt_list.map_s Repo.find_workable database_labels ||> CCList.flatten
  in
  if CCList.is_empty pending_job_instances
  then Lwt.return_unit
  else (
    Logs.info ~src (fun m ->
      m
        ~tags
        "Start working queue of length %d"
        (CCList.length pending_job_instances));
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
    Logs.info ~src (fun m -> m ~tags "Finish working queue");
    Lwt.return_unit)
;;

let create_schedule () =
  let open Utils.Lwt_result.Infix in
  let open Schedule in
  let interval = Every (Ptime.Span.of_int_s 1 |> ScheduledTimeSpan.of_span) in
  let periodic_fcn () =
    let%lwt database_labels =
      Pool_tenant.find_all ()
      ||> CCList.map (fun { Pool_tenant.database_label; _ } -> database_label)
    in
    let database_labels = Database.root :: database_labels in
    Logs.debug ~src (fun m ->
      m
        ~tags
        "Running Queue for databases: %s"
        ([%show: Database.Label.t list] database_labels));
    let jobs = !registered_jobs in
    if CCList.is_empty jobs
    then (
      Logs.debug ~src (fun m ->
        m ~tags "No jobs found to run, trying again later");
      Lwt.return_unit)
    else (
      let job_strings = jobs |> CCList.map (fun (job : job') -> job.name) in
      Logs.debug ~src (fun m ->
        m
          ~tags
          "Run job queue with registered jobs: %s"
          ([%show: string list] job_strings));
      work_queue database_labels jobs)
  in
  create "job_queue" interval periodic_fcn
;;

let start = create_schedule %> Schedule.add_and_start

let stop () =
  registered_jobs := [];
  Lwt.return_unit
;;

let lifecycle =
  Sihl.Container.create_lifecycle
    "Multitenant Queue"
    ~dependencies:(fun () ->
      [ Pool_canary.lifecycle
      ; Database.Root.lifecycle
      ; Database.Tenant.lifecycle
      ; Schedule.lifecycle
      ])
    ~start
    ~stop
;;

let register ?(jobs = []) () =
  Repo.register_migration ();
  Repo.register_cleaner ();
  registered_jobs := !registered_jobs @ jobs;
  let configuration = Sihl.Configuration.make () in
  Sihl.Container.Service.create ~configuration lifecycle
;;

module History = struct
  include Entity_history

  let create_from_queue_instance
    database_label
    { entity_uuids; message_template }
    (job_instance : instance)
    =
    entity_uuids
    |> Lwt_list.iter_s (fun entity_uuid ->
      create ?message_template ~entity_uuid job_instance
      |> Repo_history.insert database_label)
  ;;

  let query_by_entity = Repo_history.query_by_entity
  let find_related = Repo_history.find_related
end
