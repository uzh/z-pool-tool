open CCFun
include Entity

module Repo = struct
  include Repo
  include Repo_entity
end

module Guard = Entity_guard

let log_src = Logs.Src.create "pool_queue.service"

module Logs = (val Logs.src_log log_src : Logs.LOG)

let registered_jobs : AnyJob.t list ref = ref []

let register_jobs jobs =
  registered_jobs := CCList.concat [ !registered_jobs; jobs ];
  Lwt.return_unit
;;

include Repo.MakeColumns (struct
    let name = None
  end)

let find = Repo.find
let find_by = Repo.find_by
let count_workable = Repo.count_workable
let count_all_workable = Repo.count_all_workable
let find_instances_by_entity = Repo_mapping.find_instances_by_entity
let find_related = Repo_mapping.find_related

let update_and_return ?history database_label job =
  let%lwt () = Repo.update ?history database_label job in
  Lwt.return job
;;

let handle label = Instance.handle %> update_and_return label

let fail label retry_delay job =
  Instance.fail retry_delay job %> update_and_return label
;;

let success label = Instance.success %> update_and_return label

let archive instance =
  let open Status in
  match instance.Instance.status with
  | Failed | Succeeded | Cancelled -> Repo.archive instance
  | Pending -> Lwt.return_unit
;;

let dev_dispatch
  ~callback
  ?tags
  { Job.handle; decode; _ }
  ({ Instance.database_label; input; _ } as instance)
  =
  let open Utils.Lwt_result.Infix in
  Logs.info (fun m -> m ?tags "Skipping queue");
  Logs.debug (fun m ->
    m
      ?tags
      "Environment is not 'production' and/or var `QUEUE_FORCE_ASYNC` not set");
  match%lwt decode input |> Lwt_result.lift >>= handle database_label with
  | Ok () -> callback instance
  | Error msg ->
    let job = Instance.(instance |> add_error msg |> failed) in
    Logs.err (fun m -> m ?tags "Job failed: %s" ([%show: Instance.t] job));
    Lwt.return_unit
;;

let dispatch
  ?(id = Id.create ())
  ?(callback = fun (_ : 'a) -> Lwt.return_unit)
  ?message_template
  ?job_ctx
  ?run_at
  label
  input
  job
  =
  let tags = Database.Logger.Tags.create label in
  let config = Sihl.Configuration.read schema in
  let clone_of =
    CCOption.map_or
      ~default:None
      (function
        | Clone id -> Some id
        | Create _ -> None)
      job_ctx
  in
  let instance =
    Job.to_instance ~id ?message_template ?run_at ?clone_of label input job
  in
  if Sihl.Configuration.is_production () || config.force_async
  then (
    Logs.debug (fun m -> m ~tags "Dispatching job %a" JobName.pp (Job.name job));
    let%lwt () = Repo.enqueue label instance in
    let%lwt () =
      CCOption.map_or
        ~default:Lwt.return_unit
        (function
          | Clone id -> Repo_mapping.duplicate_for_new_job label id
          | Create entity_uuids ->
            Lwt_list.iter_s
              (Entity_mapping.(create instance %> to_write)
               %> Repo_mapping.insert label)
              entity_uuids)
        job_ctx
    in
    callback instance)
  else dev_dispatch ~callback ~tags job instance
;;

let dispatch_all
  ?(callback = fun (_ : 'a) -> Lwt.return_unit)
  ?run_at
  label
  inputs
  job
  =
  let tags = Database.Logger.Tags.create label in
  let config = Sihl.Configuration.read schema in
  let instances, create, clone =
    CCList.fold_left
      (fun (init_instances, init_create, init_clone)
        (id, input, message_template, job_ctx) ->
        let instance =
          Job.to_instance ~id ?message_template ?run_at label input job
        in
        match job_ctx with
        | Create uuids ->
          ( CCList.cons' init_instances instance
          , init_create @ CCList.map (Entity_mapping.create instance) uuids
          , init_clone )
        | Clone uuid ->
          ( CCList.cons' init_instances instance
          , init_create
          , CCList.cons' init_clone uuid ))
      ([], [], [])
      inputs
  in
  if Sihl.Configuration.is_production () || config.force_async
  then (
    let%lwt () = Repo.enqueue_all label instances in
    let%lwt () = Repo_mapping.insert_all label create in
    let%lwt () =
      Lwt_list.iter_s (Repo_mapping.duplicate_for_new_job label) clone
    in
    Lwt_list.iter_s callback instances)
  else Lwt_list.iter_s (dev_dispatch ~callback ~tags job) instances
;;

let run_job
  ?tags
  { AnyJob.handle; failed; _ }
  ({ Instance.id; database_label; input; _ } as instance)
  =
  let tags =
    let open Database.Logger.Tags in
    CCOption.map_or ~default:(create database_label) (add database_label) tags
  in
  let log_reraise message exn =
    Logs.err (fun m -> m ~tags "%s:\n'%s'" message (Printexc.to_string exn));
    Lwt.reraise exn
  in
  let%lwt result =
    Lwt.catch
      (fun () -> handle database_label input)
      (log_reraise
         "Exception caught while running job, this is a bug in your job \
          handler. Don't throw exceptions there, use CCResult.t instead.")
  in
  match result with
  | Error msg ->
    Lwt.catch
      (fun () ->
        let%lwt () = failed database_label msg instance in
        Lwt.return_error msg)
      (log_reraise
         "Exception caught while cleaning up job, this is a bug in your job \
          failure handler, make sure to not throw exceptions there.")
  | Ok () ->
    Logs.debug (fun m -> m "Successfully ran instance: %s" (Id.show id));
    Lwt.return_ok ()
;;

let work_job job instance =
  let database_label = Instance.database_label instance in
  let tags = Database.Logger.Tags.create database_label in
  if Instance.should_run ~is_polled:true instance
  then (
    let fail = fail database_label (AnyJob.retry_delay job) instance in
    let%lwt instance =
      Lwt.catch
        (fun () ->
          let%lwt instance = handle database_label instance in
          match%lwt run_job ~tags job instance with
          | Error msg -> fail msg
          | Ok () -> success database_label instance)
        (Printexc.to_string %> Pool_message.Error.nothandled %> fail)
    in
    let%lwt () = archive instance in
    Notifier.job_reporter instance)
  else (
    Logs.debug (fun m ->
      m ~tags "Instance shouldn't run: %s" (instance |> Instance.id |> Id.show));
    Lwt.return_unit)
;;

let work_queue (job : AnyJob.t) (database_label : Database.Label.t) =
  let tags = Database.Logger.Tags.create database_label in
  let msg_prefix = [%string "Queue %{JobName.show job.AnyJob.name}"] in
  let config = Sihl.Configuration.read schema in
  match%lwt Repo.count_workable job.AnyJob.name database_label with
  | Error msg ->
    let msg = Pool_message.Error.show msg in
    Logs.debug (fun m -> m ~tags "%s failed: %s" msg_prefix msg);
    Lwt.return_unit
  | Ok 0 ->
    Logs.debug (fun m -> m ~tags "%s: Nothing pending" msg_prefix);
    Lwt.return_unit
  | Ok count ->
    Logs.debug (fun m -> m ~tags "%s count: %d" msg_prefix count);
    let%lwt instances =
      Repo.poll_n_workable database_label config.batch_size job.AnyJob.name
    in
    let () = Lwt.async (fun () -> Lwt_list.iter_s (work_job job) instances) in
    Lwt.return_unit
;;

let create_schedule (database_label, (job : AnyJob.t)) : Schedule.t =
  let open Schedule in
  let tags = Database.Logger.Tags.create database_label in
  let interval = Every (Ptime.Span.of_int_s 1 |> ScheduledTimeSpan.of_span) in
  let periodic_fcn () =
    Logs.debug (fun m ->
      m
        ~tags
        "Running queue (JobName: %s) for databases: %s"
        ([%show: JobName.t] job.AnyJob.name)
        ([%show: Database.Label.t] database_label));
    work_queue job database_label
  in
  create
    [%string
      "queue [%{Database.Label.value database_label}]: %{JobName.show \
       job.AnyJob.name}"]
    interval
    (Some database_label)
    periodic_fcn
;;

let start () =
  let tags = Database.Logger.Tags.create Database.Pool.Root.label in
  let database_labels =
    Database.(Pool.Tenant.all ~status:[ Status.Active ] ())
  in
  Logs.info (fun m ->
    m
      ~tags
      "Start queue for databases: %s"
      ([%show: Database.Label.t list] database_labels));
  let archive_and_reset database_label =
    let%lwt () = Repo.archive_all_processed database_label in
    Repo.reset_pending_jobs database_label
  in
  let handle fcn database_label =
    try%lwt fcn database_label with
    | Caqti_error.(Exn #load_or_connect) ->
      Logs.warn (fun m ->
        m
          "Could not archive and reset jobs for database: %s"
          (Database.Label.show database_label));
      Lwt.return_unit
    | err -> raise err
  in
  let%lwt () = Lwt_list.iter_s (handle archive_and_reset) database_labels in
  match !registered_jobs with
  | [] ->
    Logs.warn (fun m -> m ~tags "No jobs registered");
    Lwt.return_unit
  | jobs ->
    let jobs_per_database =
      let open CCList in
      fold_left
        (fun jobs_per_db job ->
          let database_labels =
            match job.AnyJob.execute_on_root with
            | false -> database_labels
            | true -> Database.Pool.Root.label :: database_labels
          in
          fold_left (fun acc label -> acc @ [ label, job ]) [] database_labels
          |> append jobs_per_db)
        []
        jobs
    in
    jobs_per_database
    |> Lwt_list.iter_s (fun ((database_label, _) as job) ->
      let tags = Database.Logger.Tags.create database_label in
      create_schedule job |> Schedule.add_and_start ~tags)
;;

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
  registered_jobs := !registered_jobs @ jobs;
  let configuration = Sihl.Configuration.make () in
  Sihl.Container.Service.create
    ~configuration
    (match kind with
     | Service -> lifecycle_service
     | Worker -> lifecycle_worker)
;;
