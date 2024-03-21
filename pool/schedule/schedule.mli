module Label : sig
  include Pool_common.Model.StringSig
end

module Status : sig
  type t =
    | Active
    | Finished
    | Paused
    | Running
    | Stopped

  val create : string -> (t, Pool_common.Message.error) result
  val init : t
  val all : t list
  val schema : unit -> ('a, t) Pool_common.Utils.PoolConformist.Field.t
  val equal : t -> t -> bool
  val pp : Format.formatter -> t -> unit
  val show : t -> string
  val t_of_yojson : Yojson.Safe.t -> t
  val yojson_of_t : t -> Yojson.Safe.t
  val sexp_of_t : t -> Ppx_sexp_conv_lib.Sexp.t
end

module LastRunAt : Pool_common.Model.PtimeSig

module ScheduledTime : sig
  include Pool_common.Model.PtimeSig

  val create : Ptime.t -> (t, Pool_common.Message.error) result
end

module ScheduledTimeSpan : Pool_common.Model.PtimeSpanSig

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

val create : string -> scheduled_time -> (unit -> unit Lwt.t) -> t

type public =
  { label : Label.t
  ; scheduled_time : scheduled_time
  ; status : Status.t
  ; last_run : LastRunAt.t option
  }

val add_and_start : t -> unit Lwt.t
val stop : unit -> unit Lwt.t
val lifecycle : Sihl.Container.lifecycle
val register : ?schedules:t list -> unit -> Sihl.Container.Service.t
val is_ok : public -> bool
val find_all : unit -> public list Lwt.t
val find_by : Query.t -> (public list * Query.t) Lwt.t

module Guard : sig
  module Access : sig
    val index : Guard.ValidationSet.t
  end
end

val column_label : Query.Column.t
val column_scheduled_time : Query.Column.t
val column_status : Query.Column.t
val column_last_run_at : Query.Column.t
val default_query : Query.t
val filterable_by : Query.Filter.human option
val searchable_by : Query.Column.t list
val sortable_by : Query.Column.t list
