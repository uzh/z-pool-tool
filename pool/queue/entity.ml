open CCFun.Infix

let default_tries = 5
let default_retry_delay = Sihl.Time.Span.minutes 1

module Id : module type of Pool_common.Id = Pool_model.Base.Id

module JobName = struct
  module Core = struct
    let field = Pool_message.Field.Name

    type t =
      | CheckMatchesFilter [@name "check_matches_filter"]
      [@printer Utils.ppx_printer "check_matches_filter"]
      | SendEmail [@name "send_email"] [@printer Utils.ppx_printer "send_email"]
      | SendTextMessage [@name "send_text_message"]
      [@printer Utils.ppx_printer "send_text_message"]
    [@@deriving enum, eq, ord, sexp_of, show { with_path = false }, yojson]
  end

  include Pool_model.Base.SelectorType (Core)
  include Core
end

module Status = struct
  module Core = struct
    let field = Pool_message.Field.Status

    type t =
      | Pending [@name "pending"] [@printer Utils.ppx_printer "pending"]
      | Succeeded [@name "succeeded"] [@printer Utils.ppx_printer "succeeded"]
      | Failed [@name "failed"] [@printer Utils.ppx_printer "failed"]
      | Cancelled [@name "cancelled"] [@printer Utils.ppx_printer "cancelled"]
    [@@deriving enum, eq, ord, sexp_of, show { with_path = false }, yojson]
  end

  include Pool_model.Base.SelectorType (Core)
  include Core
end

module Instance = struct
  type t =
    { id : Id.t
    ; name : JobName.t
    ; input : string
    ; tries : int
    ; next_run_at : Ptime.t
    ; max_tries : int
    ; status : Status.t
    ; last_error : string option
    ; last_error_at : Ptime.t option
    ; tag : string option
    ; ctx : (string * string) list
    }
  [@@deriving eq, fields, show]

  let is_pending { status; _ } = Status.(equal status Pending)

  let resendable job =
    if is_pending job then Error Pool_message.Error.JobPending else Ok job
  ;;

  let should_run { tries; max_tries; next_run_at; status; _ } now =
    let has_tries_left = tries < max_tries in
    let is_after_delay = not (Ptime.is_later next_run_at ~than:now) in
    let is_pending =
      let open Status in
      match status with
      | Pending -> true
      | Succeeded | Failed | Cancelled -> false
    in
    is_pending && has_tries_left && is_after_delay
  ;;

  let default_error_handler label msg { id; name; input; _ } =
    Logs.err (fun m ->
      m
        "[label: %a] Job with id '%a' and name '%a' failed for input '%s': %s"
        Database.Label.pp
        label
        Id.pp
        id
        JobName.pp
        name
        input
        msg);
    Lwt.return_unit
  ;;

  let update_next_run_at (retry_delay : Ptime.Span.t) (job_instance : t) =
    let next_run_at =
      match Ptime.add_span job_instance.next_run_at retry_delay with
      | Some date -> date
      | None -> failwith "Can not determine next run date of job"
    in
    { job_instance with next_run_at }
  ;;

  let increment_tries
    (retry_delay : Ptime.Span.t)
    ({ next_run_at; tries; _ } as instance)
    =
    let next_run_at =
      match Ptime.add_span next_run_at retry_delay with
      | Some date -> date
      | None -> failwith "Can not determine next run date of job"
    in
    { instance with tries = tries + 1; next_run_at }
  ;;

  let create
    ?(id = Id.create ())
    ?(tries = 0)
    ?(max_tries = default_tries)
    ?(status = Status.Pending)
    ?last_error
    ?last_error_at
    ?delay
    ?(now = Ptime_clock.now ())
    ?tag
    ctx
    name
    input
    =
    let next_run_at =
      match delay with
      | Some delay -> CCOption.value (Ptime.add_span now delay) ~default:now
      | None -> now
    in
    { id
    ; name
    ; input
    ; tries
    ; next_run_at
    ; max_tries
    ; status
    ; last_error
    ; last_error_at
    ; tag
    ; ctx
    }
  ;;
end

module Job = struct
  type 'a t =
    { name : JobName.t
    ; encode : 'a -> string
    ; decode : string -> ('a, string) result
    ; handle : Database.Label.t -> 'a -> (unit, string) Lwt_result.t
    ; failed : Database.Label.t -> string -> Instance.t -> unit Lwt.t
    ; max_tries : int
    ; retry_delay : Ptime.Span.t
    ; tag : string option
    }
  [@@deriving fields, show]

  let create
    ?(max_tries = default_tries)
    ?(retry_delay = default_retry_delay)
    ?(failed = Instance.default_error_handler)
    ?tag
    handle
    encode
    decode
    name
    =
    { name; handle; failed; max_tries; retry_delay; encode; decode; tag }
  ;;

  let to_instance
    label
    input
    delay
    now
    ({ name; encode; max_tries; tag; _ } : 'a t)
    =
    Instance.create
      ?delay
      ~now
      ~max_tries
      ?tag
      (Database.to_ctx label)
      name
      (encode input)
  ;;
end

module AnyJob = struct
  type t =
    { name : JobName.t
    ; handle : Database.Label.t -> string -> (unit, string) Lwt_result.t
    ; failed : Database.Label.t -> string -> Instance.t -> unit Lwt.t
    ; max_tries : int
    ; retry_delay : Ptime.Span.t
    }
  [@@deriving fields, show]
end

let hide (job : 'a Job.t) : AnyJob.t =
  let handle label input =
    match job.Job.decode input with
    | Ok decoded -> job.Job.handle label decoded
    | Error msg -> Lwt.return_error msg
  in
  { AnyJob.name = job.Job.name
  ; handle
  ; failed = job.Job.failed
  ; max_tries = job.Job.max_tries
  ; retry_delay = job.Job.retry_delay
  }
;;

type config =
  { force_async : bool
  ; process_queue : bool
  }

let config force_async process_queue = { force_async; process_queue }

let schema =
  let open Conformist in
  make
    Field.
      [ bool
          ~meta:"If set to true, the queue is used even in development."
          ~default:false
          "QUEUE_FORCE_ASYNC"
      ; bool
          ~meta:"If set to false, jobs can be dispatched but won't be handled."
          ~default:true
          "QUEUE_PROCESS"
      ]
    config
;;

open Pool_message

let is_pending = Instance.status %> Status.(equal Pending)

let resendable job =
  let open CCResult in
  let* () = if is_pending job then Error Error.JobPending else Ok () in
  let* () =
    let open JobName in
    Instance.name job
    |> function
    | CheckMatchesFilter -> Error Error.JobCannotBeRetriggered
    | SendEmail | SendTextMessage -> Ok ()
  in
  Ok job
;;

let column_job_name = (Field.Name, "queue_jobs.name") |> Query.Column.create

let column_job_status =
  (Field.Status, "queue_jobs.status") |> Query.Column.create
;;

let column_last_error =
  (Field.LastError, "queue_jobs.last_error") |> Query.Column.create
;;

let column_last_error_at =
  (Field.LastErrorAt, "queue_jobs.last_error_at") |> Query.Column.create
;;

let column_next_run =
  (Field.NextRunAt, "queue_jobs.next_run_at") |> Query.Column.create
;;

let column_input = (Field.Input, "queue_jobs.input") |> Query.Column.create

let build_options all show =
  let languages = Pool_common.Language.all in
  all
  |> CCList.map (fun item ->
    let label =
      languages
      |> CCList.map (fun lang -> lang, show item |> CCString.capitalize_ascii)
    in
    let value = show item in
    Query.Filter.SelectOption.create label value)
;;

let job_name_filter =
  let open Query.Filter in
  let open JobName in
  let options = build_options all show in
  Condition.Human.Select (column_job_name, options)
;;

let job_status_filter =
  let open Query.Filter in
  let open Status in
  let options = build_options all show in
  Condition.Human.Select (column_job_status, options)
;;

let searchable_by = [ column_input ]

let sortable_by =
  [ column_job_name
  ; column_job_status
  ; column_last_error
  ; column_last_error_at
  ; column_next_run
  ]
;;

let filterable_by = Some [ job_name_filter; job_status_filter ]

let default_sort =
  Query.Sort.{ column = column_next_run; order = SortOrder.Descending }
;;

let default_query = Query.create ~sort:default_sort ()
