open CCFun

module Label = struct
  include Pool_common.Model.String

  let field = Pool_common.Message.Field.Label
  let schema = schema ?validation:None field
end

module ScheduledTime = struct
  include Pool_common.Model.Ptime

  let field = Pool_common.Message.Field.ScheduledTime
  let create m = Ok m
  let schema = schema field create
end

module ScheduledTimeSpan = struct
  include Pool_common.Model.PtimeSpan

  let create m =
    if Ptime.Span.abs m |> Ptime.Span.equal m
    then Ok m
    else Error Pool_common.Message.NegativeAmount
  ;;

  let field = Pool_common.Message.Field.ScheduledTimeSpan
  let schema = schema field create
end

module LastRunAt = struct
  include Pool_common.Model.Ptime

  let field = Pool_common.Message.Field.LastRunAt
  let create m = Ok m
  let schema = schema field create
end

module Status = struct
  module Core = struct
    let field = Pool_common.Message.Field.Status
    let go m fmt _ = Format.pp_print_string fmt m

    type t =
      | Active [@name "active"] [@printer go "active"]
      | Finished [@name "finished"] [@printer go "finished"]
      | Paused [@name "paused"] [@printer go "paused"]
      | Running [@name "running"] [@printer go "running"]
      | Stopped [@name "stopped"] [@printer go "stopped"]
    [@@deriving enum, eq, ord, show { with_path = false }, yojson, sexp_of]
  end

  include Pool_common.Model.SelectorType (Core)
  include Core

  let init = Active
end

type scheduled_time =
  | Every of ScheduledTimeSpan.t
  | At of ScheduledTime.t
[@@deriving eq, show]

type t =
  { label : Label.t
  ; scheduled_time : scheduled_time
  ; status : Status.t
  ; last_run : LastRunAt.t option
  ; fcn : unit -> unit Lwt.t [@opaque] [@equal fun _ _ -> true]
  }
[@@deriving eq, show]

let create label scheduled_time fcn =
  { label; scheduled_time; status = Status.init; last_run = None; fcn }
;;

let run_in = function
  | At time ->
    Ptime.diff time (Ptime_clock.now ()) |> Ptime.Span.to_float_s |> max 1.
  | Every duration ->
    duration |> ScheduledTimeSpan.value |> Ptime.Span.to_float_s
;;

type public =
  { label : Label.t
  ; scheduled_time : scheduled_time
  ; status : Status.t
  ; last_run : LastRunAt.t option
  }
[@@deriving eq, show]
