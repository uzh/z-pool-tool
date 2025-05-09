module Label = struct
  include Pool_model.Base.String

  let field = Pool_message.Field.Label
  let schema = schema ?validation:None field
end

module ScheduledTime = struct
  include Pool_model.Base.Ptime

  let create m = Ok m
  let field = Pool_message.Field.ScheduledTime
  let schema = schema field create
end

module ScheduledTimeSpan = struct
  include Pool_model.Base.PtimeSpan

  let create m =
    if Ptime.Span.abs m |> Ptime.Span.equal m
    then Ok m
    else Error Pool_message.Error.NegativeAmount
  ;;

  let field = Pool_message.Field.ScheduledTimeSpan
  let schema = schema field create
end

module LastRunAt = struct
  include Pool_model.Base.Ptime

  let field = Pool_message.Field.LastRunAt
  let schema = schema field CCFun.(create %> CCResult.return)
end

module Status = struct
  module Core = struct
    let field = Pool_message.Field.Status
    let go m fmt _ = Format.pp_print_string fmt m

    type t =
      | Active [@name "active"] [@printer go "active"]
      | Failed [@name "failed"] [@printer go "failed"]
      | Finished [@name "finished"] [@printer go "finished"]
      | Paused [@name "paused"] [@printer go "paused"]
      | Running [@name "running"] [@printer go "running"]
      | Stopped [@name "stopped"] [@printer go "stopped"]
    [@@deriving enum, eq, ord, show { with_path = false }, yojson, sexp_of]
  end

  include Pool_model.Base.SelectorType (Core)
  include Core

  let init = Active
end

type scheduled_time =
  | Every of ScheduledTimeSpan.t
  | At of ScheduledTime.t
[@@deriving eq, show]

type t =
  { label : Label.t
  ; database_label : Database.Label.t option
  ; scheduled_time : scheduled_time
  ; status : Status.t
  ; last_run : LastRunAt.t option
  ; fcn : unit -> unit Lwt.t [@opaque] [@equal fun _ _ -> true]
  }
[@@deriving eq, show]

let create label scheduled_time database_label fcn =
  { label; database_label; scheduled_time; status = Status.init; last_run = None; fcn }
;;

let run_in = function
  | At time -> Ptime.diff time (Ptime_clock.now ()) |> Ptime.Span.to_float_s |> max 1.
  | Every duration -> duration |> ScheduledTimeSpan.value |> Ptime.Span.to_float_s
;;

type public =
  { label : Label.t
  ; scheduled_time : scheduled_time
  ; status : Status.t
  ; last_run : LastRunAt.t option
  }
[@@deriving eq, show]

let is_ok ({ scheduled_time; status; last_run; _ } : public) =
  let open Status in
  let is_fine = function
    | Finished | Paused | Stopped -> true
    | Active | Running | Failed -> false
  in
  let did_run () =
    match scheduled_time, last_run with
    | (At _ | Every _), None -> false
    | At _, Some _ -> true
    | Every interval, Some last_run ->
      let minimum_interval =
        let default = 60 in
        CCOption.(
          Ptime.Span.to_int_s interval
          >|= CCInt.(max default)
          |> value ~default
          |> Ptime.Span.of_int_s)
      in
      Ptime.add_span last_run (Ptime.Span.add minimum_interval minimum_interval)
      |> CCOption.map_or ~default:false (Ptime.is_later ~than:(Ptime_clock.now ()))
  in
  is_fine status || did_run ()
;;

open Pool_message

let column_label = (Field.Label, "pool_schedules.label") |> Query.Column.create

let column_scheduled_time =
  (Field.ScheduledTime, "pool_schedules.scheduled_time") |> Query.Column.create
;;

let column_status = (Field.Status, "pool_schedules.status") |> Query.Column.create

let column_last_run_at =
  (Field.LastRunAt, "pool_schedules.last_run_at") |> Query.Column.create
;;

let column_created_at =
  (Field.CreatedAt, "pool_schedules.created_at") |> Query.Column.create
;;

let filterable_by = None
let searchable_by = [ column_label ]
let sortable_by = [ column_scheduled_time; column_status; column_last_run_at ]
let sortable_by = (column_created_at :: searchable_by) @ sortable_by
let default_sort = Query.Sort.{ column = column_label; order = SortOrder.Descending }
let default_query = Query.create ~sort:default_sort ()
