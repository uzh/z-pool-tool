module Label : sig
  include Pool_common.Model.StringSig
end

module Status : sig
  type t =
    | Active
    | Finished
    | Paused
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

module LastRun : Pool_common.Model.PtimeSig
module ScheduledTime : Pool_common.Model.PtimeSig
module ScheduledTimeSpan : Pool_common.Model.PtimeSpanSig

type scheduled_time =
  | Every of ScheduledTimeSpan.t
  | At of ScheduledTime.t
[@@deriving eq, show]

type t =
  { label : Label.t
  ; scheduled_time : scheduled_time
  ; status : Status.t
  ; last_run : LastRun.t option
  ; fcn : unit -> unit Lwt.t [@opaque] [@equal fun _ _ -> true]
  }

module RegisteredSchedules : sig
  type t = Entity.t

  val find_all : unit -> t list
  val iter : (t -> unit) -> unit
  val add : t -> unit Lwt.t
  val add_multiple : t list -> unit Lwt.t
  val update_last_run : t -> unit Lwt.t
  val finish : t -> unit Lwt.t
  val update_run_status : scheduled_time -> t -> unit Lwt.t
  val stop_all_active : unit -> unit Lwt.t
end

val start : 'a -> unit Lwt.t
val stop : 'a -> unit Lwt.t
val lifecycle : Sihl.Container.lifecycle
val register : t list -> Sihl.Container.Service.t Lwt.t
