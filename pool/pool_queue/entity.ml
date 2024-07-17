open Pool_message
open Ppx_yojson_conv_lib.Yojson_conv.Primitives

let default_tries = 5
let default_retry_delay = Sihl.Time.Span.minutes 1

module Id : module type of Pool_common.Id = Pool_model.Base.Id

module JobName = struct
  module Core = struct
    let field = Field.Name

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
    let field = Field.Status

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

open Status

module PersistedAt = struct
  include Pool_model.Base.Ptime
end

module PolledAt = struct
  include Pool_model.Base.Ptime
end

module HandledAt = struct
  include Pool_model.Base.Ptime
end

type run_at =
  | Delay of Ptime.span
  | Now

module RunAt = struct
  include Pool_model.Base.Ptime

  let of_run_at =
    let now = Ptime_clock.now () in
    function
    | Delay delay ->
      Ptime.add_span now delay
      |> CCOption.get_exn_or "Could not add delay for job."
    | Now -> now
  ;;
end

module ErrorAt = struct
  include Pool_model.Base.Ptime
end

module Instance = struct
  type t =
    { id : Id.t
    ; name : JobName.t
    ; input : string
    ; message_template : string option
    ; tries : int
    ; max_tries : int
    ; run_at : RunAt.t
    ; status : Status.t
    ; persisted_at : PersistedAt.t
    ; polled_at : PolledAt.t option
    ; handled_at : HandledAt.t option
    ; last_error : string option
    ; last_error_at : ErrorAt.t option
    ; database_label : Database.Label.t
    ; clone_of : Id.t option
    }
  [@@deriving eq, fields, show]

  let is_pending { status; _ } = Status.(equal status Pending)
  let is_polled { polled_at; _ } = CCOption.is_none polled_at

  let currently_handled { polled_at; handled_at; _ } =
    match polled_at, handled_at with
    | None, _ -> false
    | Some _, None -> true
    | Some polled_at, Some handled_at ->
      Ptime.is_later ~than:handled_at polled_at
  ;;

  let should_run
    ?(is_polled = false)
    ({ tries; max_tries; run_at; polled_at; _ } as job)
    =
    let has_tries_left = tries < max_tries in
    let is_after_delay = Ptime_clock.now () |> Ptime.is_later ~than:run_at in
    let is_pending = is_pending job in
    is_pending
    && has_tries_left
    && is_after_delay
    && CCOption.is_some polled_at == is_polled
  ;;

  let resendable job =
    let open CCResult in
    let* () =
      if is_pending job || currently_handled job
      then Error Error.JobPending
      else Ok ()
    in
    let* () =
      let open JobName in
      name job
      |> function
      | CheckMatchesFilter -> Error Error.JobCannotBeRetriggered
      | SendEmail | SendTextMessage -> Ok ()
    in
    Ok job
  ;;

  let create
    ?(id = Id.create ())
    ?message_template
    ?(tries = 0)
    ?(max_tries = default_tries)
    ?(status = Status.Pending)
    ?last_error
    ?last_error_at
    ?(run_at = Now)
    ?clone_of
    database_label
    name
    input
    =
    { id
    ; name
    ; input
    ; message_template
    ; run_at = RunAt.of_run_at run_at
    ; tries
    ; max_tries
    ; status
    ; persisted_at = PersistedAt.create_now ()
    ; polled_at = None
    ; handled_at = None
    ; last_error
    ; last_error_at
    ; database_label
    ; clone_of
    }
  ;;

  let retry (retry_delay : Ptime.Span.t) (job : t) =
    let run_at =
      match Ptime.add_span job.run_at retry_delay with
      | Some date -> date
      | None -> failwith "Can not determine next run date of job"
    in
    { job with run_at }
  ;;

  let increment_tries ({ tries; _ } as job) = { job with tries = tries + 1 }

  let add_error error job =
    { job with
      last_error = Some (Pool_message.Error.show error)
    ; last_error_at = Some (ErrorAt.create_now ())
    }
  ;;

  let cancelled job = { job with status = Cancelled }
  let failed job = { job with status = Failed }
  let succeeded job = { job with status = Succeeded }
  let poll job = { job with polled_at = Some (PolledAt.create_now ()) }

  let handle job =
    increment_tries { job with handled_at = Some (HandledAt.create_now ()) }
  ;;

  let fail delay ({ tries; max_tries; _ } as job) error =
    job |> add_error error |> if tries >= max_tries then failed else retry delay
  ;;

  let success = succeeded

  let recreate ?run_at job =
    create
      ?message_template:job.message_template
      ~max_tries:job.max_tries
      ?run_at
      job.database_label
      job.name
      job.input
  ;;

  let default_error_handler label msg instance =
    let tags = Database.Logger.Tags.create label in
    let job = instance |> add_error msg |> failed in
    Logs.err (fun m -> m ~tags "Job failed: %s" ([%show: t] job));
    Lwt.return_unit
  ;;
end

module Job = struct
  type 'a t =
    { name : JobName.t
    ; encode : 'a -> string
    ; decode : string -> ('a, Pool_message.Error.t) result
    ; handle :
        Database.Label.t -> 'a -> (unit, Pool_message.Error.t) Lwt_result.t
    ; failed :
        Database.Label.t -> Pool_message.Error.t -> Instance.t -> unit Lwt.t
    ; max_tries : int
    ; retry_delay : Ptime.Span.t
    }
  [@@deriving fields, show]

  let decode { decode; _ } = decode

  let create
    ?(max_tries = default_tries)
    ?(retry_delay = default_retry_delay)
    ?(failed = Instance.default_error_handler)
    handle
    encode
    decode
    name
    =
    { name; handle; failed; max_tries; retry_delay; encode; decode }
  ;;

  let to_instance
    ?id
    ?message_template
    ?run_at
    ?clone_of
    label
    input
    ({ name; encode; max_tries; _ } : 'a t)
    =
    Instance.create
      ?id
      ?message_template
      ?run_at
      ?clone_of
      ~max_tries
      label
      name
      (encode input)
  ;;
end

type mappings =
  | Create of Pool_common.Id.t list
  | Clone of Id.t
[@@deriving eq, show, yojson]

let mappings_create ids =
  Create (ids |> CCList.stable_sort Pool_common.Id.compare)
;;

let mappings_clone id = Clone id

module AnyJob = struct
  type t =
    { name : JobName.t
    ; handle :
        Database.Label.t -> string -> (unit, Pool_message.Error.t) Lwt_result.t
    ; failed :
        Database.Label.t -> Pool_message.Error.t -> Instance.t -> unit Lwt.t
    ; max_tries : int
    ; retry_delay : Ptime.Span.t
    }
  [@@deriving fields, show]
end

let hide (job : 'a Job.t) : AnyJob.t =
  let open Utils.Lwt_result.Infix in
  let handle label input =
    job.Job.decode input |> Lwt_result.lift >>= job.Job.handle label
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
