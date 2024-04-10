module JobName = struct
  module Core = struct
    let field = Pool_message.Field.Name

    type t =
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

type instance =
  { id : string
  ; name : string
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
[@@deriving show]

type 'a job =
  { name : string
  ; encode : 'a -> string
  ; decode : string -> ('a, string) Result.t
  ; handle : Database.Label.t -> 'a -> (unit, string) Result.t Lwt.t
  ; failed : Database.Label.t -> string -> instance -> unit Lwt.t
  ; max_tries : int
  ; retry_delay : Ptime.Span.t
  ; tag : string option
  }
[@@deriving show]

type job' =
  { name : string
  ; handle : Database.Label.t -> string -> (unit, string) Result.t Lwt.t
  ; failed : Database.Label.t -> string -> instance -> unit Lwt.t
  ; max_tries : int
  ; retry_delay : Ptime.Span.t
  }
[@@deriving show]

let hide (job : 'a job) : job' =
  let handle label input =
    match job.decode input with
    | Ok decoded -> job.handle label decoded
    | Error msg -> Lwt.return_error msg
  in
  { name = job.name
  ; handle
  ; failed = job.failed
  ; max_tries = job.max_tries
  ; retry_delay = job.retry_delay
  }
;;

let should_run (job_instance : instance) now =
  let tries = job_instance.tries in
  let max_tries = job_instance.max_tries in
  let next_run_at = job_instance.next_run_at in
  let has_tries_left = tries < max_tries in
  let is_after_delay = not (Ptime.is_later next_run_at ~than:now) in
  let is_pending =
    let open Status in
    match job_instance.status with
    | Pending -> true
    | Succeeded | Failed | Cancelled -> false
  in
  is_pending && has_tries_left && is_after_delay
;;

let default_tries = 5
let default_retry_delay = Sihl.Time.Span.minutes 1

let default_error_handler label msg (instance : instance) =
  Logs.err (fun m ->
    m
      "[label: %a] Job with id '%s' and name '%s' failed for input '%s': %s"
      Database.Label.pp
      label
      instance.id
      instance.name
      instance.input
      msg);
  Lwt.return_unit
;;

let create_job
  handle
  ?(max_tries = default_tries)
  ?(retry_delay = default_retry_delay)
  ?(failed = default_error_handler)
  ?tag
  encode
  decode
  name
  =
  { name; handle; failed; max_tries; retry_delay; encode; decode; tag }
;;

open Pool_message

let is_pending job = Status.(equal job.status Pending)
let resendable job = if is_pending job then Error Error.JobPending else Ok job
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
