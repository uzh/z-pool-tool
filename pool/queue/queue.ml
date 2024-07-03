open CCFun
include Entity
module Guard = Entity_guard

let log_src = Logs.Src.create "queue.service"

module Logs = (val Logs.src_log log_src : Logs.LOG)

let registered_jobs : AnyJob.t list ref = ref []

let register_jobs jobs =
  registered_jobs := CCList.concat [ !registered_jobs; jobs ];
  Lwt.return_unit
;;

let find = Repo.find
let find_by = Repo.find_by
let count_workable = Repo.count_workable

let dispatch ?callback ?delay label input ({ Job.name; handle; _ } as job) =
  let tags = Database.Logger.Tags.create label in
  let config = Sihl.Configuration.read schema in
  let force_async = config.force_async in
  if Sihl.Configuration.is_production () || force_async
  then (
    Logs.debug (fun m -> m ~tags "Dispatching job %a" JobName.pp name);
    let now = Ptime_clock.now () in
    let job_instance = Job.to_instance label input delay now job in
    let%lwt () = Repo.enqueue label job_instance in
    match callback with
    | None -> Lwt.return_unit
    | Some callback -> callback job_instance)
  else (
    Logs.info (fun m -> m ~tags "Skipping queue in development environment");
    (* TODO: How to solve the this? *)
    let id = Id.create () in
    match%lwt handle label id input with
    | Ok () -> Lwt.return_unit
    | Error msg ->
      Logs.err (fun m ->
        m ~tags "Error while processing job '%a': %s" JobName.pp name msg);
      Lwt.return_unit)
;;

let dispatch_all ?callback ?delay label inputs ({ Job.name; handle; _ } as job) =
  let tags = Database.Logger.Tags.create label in
  let config = Sihl.Configuration.read schema in
  let force_async = config.force_async in
  if Sihl.Configuration.is_production () || force_async
  then (
    let now = Ptime_clock.now () in
    (* At some point we might want to have a different ctx for the job dispatch
       and the actual job handling. *)
    let job_instances =
      CCList.map (fun input -> Job.to_instance label input delay now job) inputs
    in
    let%lwt () = Repo.enqueue_all label job_instances in
    match callback with
    | None -> Lwt.return_unit
    | Some callback -> Lwt_list.iter_s callback job_instances)
  else (
    Logs.info (fun m -> m ~tags "Skipping queue in development environment");
    (* TODO: How to solve the this? *)
    let id = Id.create () in
    let rec loop inputs =
      match inputs with
      | input :: inputs ->
        Lwt.bind (handle label id input) (function
          | Ok () -> loop inputs
          | Error msg ->
            Logs.err (fun m ->
              m ~tags "Error while processing job '%a': %s" JobName.pp name msg);
            loop inputs)
      | [] -> Lwt.return_unit
    in
    loop inputs)
;;

let run_job
  ?tags
  { AnyJob.handle; failed; _ }
  ({ Instance.id; ctx; _ } as job_instance)
  : (unit, string) Lwt_result.t
  =
  let with_log_error ?tags message exn =
    let exn_string = Printexc.to_string exn in
    Logs.err (fun m -> m ?tags message exn_string);
    Lwt.return_error exn_string
  in
  let label = Database.of_ctx_exn ctx in
  let tags =
    let open Database.Logger.Tags in
    CCOption.map_or ~default:(create label) (add label) tags
  in
  let%lwt result =
    Lwt.catch
      (fun () -> handle job_instance)
      (with_log_error
         ~tags
         "Exception caught while running job, this is a bug in your job \
          handler. Don't throw exceptions there, use CCResult.t instead. '%s'")
  in
  match result with
  | Error msg ->
    Logs.err (fun m ->
      m
        ~tags
        "Failure while running job instance %s %s"
        ([%show: Instance.t] job_instance)
        msg);
    Lwt.catch
      (fun () ->
        let%lwt () = failed label msg job_instance in
        Lwt.return_error msg)
      (with_log_error
         ~tags
         "Exception caught while cleaning up job, this is a bug in your job \
          failure handler, make sure to not throw exceptions there '%s")
  | Ok () ->
    Logs.debug (fun m -> m "Successfully ran job instance '%a'" Id.pp id);
    Lwt.return_ok ()
;;

let update = Repo.update

let work_job
  ({ AnyJob.retry_delay; max_tries; _ } as job)
  ({ Instance.tries; ctx; _ } as job_instance)
  =
  let database_label = Database.of_ctx_exn ctx in
  let tags = Database.Logger.Tags.create database_label in
  let now = Ptime_clock.now () in
  if Instance.should_run job_instance now
  then (
    let%lwt job_run_status = run_job ~tags job job_instance in
    let job_instance = job_instance |> Instance.increment_tries retry_delay in
    let job_instance =
      match job_run_status with
      | Error msg when tries >= max_tries ->
        { job_instance with
          Instance.status = Status.Failed
        ; last_error = Some msg
        ; last_error_at = Some (Ptime_clock.now ())
        }
      | Error msg ->
        { job_instance with
          Instance.last_error = Some msg
        ; last_error_at = Some (Ptime_clock.now ())
        }
      | Ok () -> { job_instance with Instance.status = Status.Succeeded }
    in
    let%lwt () = Notifier.job_reporter job_instance in
    update database_label job_instance)
  else (
    Logs.debug (fun m ->
      m
        ~tags
        "Not going to run job instance %s"
        ([%show: Instance.t] job_instance));
    Lwt.return_unit)
;;

let work_queue database_labels jobs =
  let open Utils.Lwt_result.Infix in
  let open Database.Logger in
  let tags = Tags.create Database.root in
  let%lwt pending_instances =
    Lwt_list.map_s Repo.find_workable database_labels ||> CCList.flatten
  in
  if CCList.is_empty pending_instances
  then Lwt.return_unit
  else (
    Logs.info (fun m ->
      m
        ~tags
        "Start working queue of length %d"
        (CCList.length pending_instances));
    let rec loop jobs = function
      | [] -> Lwt.return_unit
      | ({ Instance.name; _ } as instance) :: instances ->
        CCList.find_opt (AnyJob.name %> JobName.equal name) jobs
        |> (function
         | None -> loop jobs instances
         | Some job -> work_job job instance)
    in
    let%lwt () = loop jobs pending_instances in
    Logs.info (fun m -> m ~tags "Finish working queue");
    Lwt.return_unit)
;;

let create_schedule () =
  let open Utils.Lwt_result.Infix in
  let open Schedule in
  let tags = Database.Logger.Tags.create Database.root in
  let interval = Every (Ptime.Span.of_int_s 1 |> ScheduledTimeSpan.of_span) in
  let periodic_fcn () =
    let%lwt database_labels =
      Pool_tenant.find_all ()
      ||> CCList.map (fun { Pool_tenant.database_label; _ } -> database_label)
    in
    let database_labels = Database.root :: database_labels in
    Logs.debug (fun m ->
      m
        ~tags
        "Running Queue for databases: %s"
        ([%show: Database.Label.t list] database_labels));
    let jobs = !registered_jobs in
    if CCList.is_empty jobs
    then (
      Logs.debug (fun m -> m ~tags "No jobs found to run, trying again later");
      Lwt.return_unit)
    else (
      let job_strings = jobs |> CCList.map AnyJob.name in
      Logs.debug (fun m ->
        m
          ~tags
          "Run job queue with registered jobs: %s"
          ([%show: JobName.t list] job_strings));
      work_queue database_labels jobs)
  in
  create "job_queue" interval periodic_fcn
;;

let start = create_schedule %> Schedule.add_and_start

let stop () =
  registered_jobs := [];
  Lwt.return_unit
;;

type kind =
  | Service
  | Worker

let lifecycle_service =
  Sihl.Container.create_lifecycle
    "Multitenant Queue"
    ~implementation_name:"service"
    ~dependencies:(fun () ->
      [ Pool_canary.lifecycle; Pool_database.lifecycle; Schedule.lifecycle ])
    ~stop
;;

let lifecycle_worker =
  Sihl.Container.create_lifecycle
    "Multitenant Queue"
    ~implementation_name:"worker"
    ~dependencies:(fun () ->
      [ Pool_canary.lifecycle; Pool_database.lifecycle; Schedule.lifecycle ])
    ~start
    ~stop
;;

let register ?(kind = Service) ?(jobs = []) () =
  Repo.register_migration ();
  Repo.register_cleaner ();
  registered_jobs := !registered_jobs @ jobs;
  let configuration = Sihl.Configuration.make () in
  Sihl.Container.Service.create
    ~configuration
    (match kind with
     | Service -> lifecycle_service
     | Worker -> lifecycle_worker)
;;

module History = struct
  include Entity_history

  let create_from_queue_instance
    database_label
    { entity_uuids; message_template }
    (job_instance : Instance.t)
    =
    entity_uuids
    |> Lwt_list.iter_s (fun entity_uuid ->
      create ?message_template ~entity_uuid job_instance
      |> Repo_history.insert database_label)
  ;;

  let query_by_entity = Repo_history.query_by_entity
  let find_related = Repo_history.find_related
end

module Repo = struct
  module Id = Repo_entity.Id
end
