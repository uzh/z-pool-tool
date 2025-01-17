module Guard = Entity_guard

type run_at =
  | Delay of Ptime.span
  | Now

module Id : sig
  include Pool_model.Base.IdSig
end

module JobName : sig
  type t =
    | CheckMatchesFilter
    | SendEmail
    | SendTextMessage

  val equal : t -> t -> bool
  val compare : t -> t -> int
  val pp : Format.formatter -> t -> unit
  val show : t -> string
  val sexp_of_t : t -> Sexplib0.Sexp.t
  val t_of_yojson : Yojson.Safe.t -> t
  val yojson_of_t : t -> Yojson.Safe.t
  val all : t list
end

module Status : sig
  type t =
    | Pending
    | Succeeded
    | Failed
    | Cancelled

  val equal : t -> t -> bool
  val compare : t -> t -> int
  val pp : Format.formatter -> t -> unit
  val show : t -> string
  val sexp_of_t : t -> Sexplib0.Sexp.t
  val t_of_yojson : Yojson.Safe.t -> t
  val yojson_of_t : t -> Yojson.Safe.t
  val all : t list
end

module Instance : sig
  type t

  val equal : t -> t -> bool
  val pp : Format.formatter -> t -> unit
  val show : t -> string
  val last_error_at : t -> Ptime.t option
  val last_error : t -> string option
  val status : t -> Status.t
  val tries : t -> int
  val max_tries : t -> int
  val run_at : t -> Ptime.t
  val clone_of : t -> Id.t option
  val message_template : t -> string option
  val input : t -> string
  val name : t -> JobName.t
  val database_label : t -> Database.Label.t
  val id : t -> Id.t
  val is_pending : t -> bool
  val resendable : t -> (t, Pool_message.Error.t) result
  val should_run : ?is_polled:bool -> t -> bool

  val create
    :  ?id:Id.t
    -> ?message_template:string
    -> ?tries:int
    -> ?max_tries:int
    -> ?status:Status.t
    -> ?last_error:string
    -> ?last_error_at:Ptime.t
    -> ?run_at:run_at
    -> ?clone_of:Id.t
    -> Database.Label.t
    -> JobName.t
    -> string
    -> t
end

module Job : sig
  type 'a t

  val pp : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit
  val show : (Format.formatter -> 'a -> unit) -> 'a t -> string
  val retry_delay : 'a t -> Ptime.span
  val max_tries : 'a t -> int

  val failed
    :  'a t
    -> Database.Label.t
    -> Pool_message.Error.t
    -> Instance.t
    -> unit Lwt.t

  val handle
    :  'a t
    -> ?id:Id.t
    -> Database.Label.t
    -> 'a
    -> (unit, Pool_message.Error.t) Lwt_result.t

  val decode : 'a t -> string -> ('a, Pool_message.Error.t) result
  val encode : 'a t -> 'a -> string
  val name : 'a t -> JobName.t

  val create
    :  ?max_tries:int
    -> ?retry_delay:Ptime.span
    -> ?failed:(Database.Label.t -> Pool_message.Error.t -> Instance.t -> unit Lwt.t)
    -> (?id:Id.t -> Database.Label.t -> 'a -> (unit, Pool_message.Error.t) Lwt_result.t)
    -> ('a -> string)
    -> (string -> ('a, Pool_message.Error.t) result)
    -> JobName.t
    -> 'a t

  val to_instance
    :  ?id:Id.t
    -> ?message_template:string
    -> ?run_at:run_at
    -> ?clone_of:Id.t
    -> Database.Label.t
    -> 'a
    -> 'a t
    -> Instance.t
end

module AnyJob : sig
  type t

  val pp : Format.formatter -> t -> unit
  val show : t -> string
  val retry_delay : t -> Ptime.span
  val max_tries : t -> int
  val failed : t -> Database.Label.t -> Pool_message.Error.t -> Instance.t -> unit Lwt.t

  val handle
    :  t
    -> ?id:Id.t
    -> Database.Label.t
    -> string
    -> (unit, Pool_message.Error.t) Lwt_result.t

  val name : t -> JobName.t
end

val find : Database.Label.t -> Id.t -> (Instance.t, Pool_message.Error.t) Lwt_result.t

val find_by
  :  [< `Current | `History ]
  -> ?query:Query.t
  -> Database.Label.t
  -> (Instance.t list * Query.t) Lwt.t

val find_instances_by_entity
  :  [< `Current | `History ]
  -> ?query:Query.t
  -> Database.Label.t
  -> Pool_common.Id.t
  -> (Instance.t list * Query.t) Lwt.t

val find_related
  :  Database.Label.t
  -> Instance.t
  -> [< `Contact | `Experiment ]
  -> Pool_common.Id.t option Lwt.t

val count_workable
  :  JobName.t
  -> Database.Label.t
  -> (int, Pool_message.Error.t) result Lwt.t

val count_all_workable : Database.Label.t -> (int, Pool_message.Error.t) result Lwt.t

include Repo.ColumnsSig

module History : sig
  type model =
    | Admin
    | Assignment
    | Contact
    | Experiment
    | Invitation
    | Session

  type item = model * Pool_common.Id.t

  val pp_item : Format.formatter -> item -> unit
  val equal_item : item -> item -> bool
  val sort : item list -> item list
end

type job_ctx =
  | Create of History.item list
  | Clone of Id.t

val equal_job_ctx : job_ctx -> job_ctx -> bool
val pp_job_ctx : Format.formatter -> job_ctx -> unit
val show_job_ctx : job_ctx -> string
val job_ctx_of_yojson : Yojson.Safe.t -> job_ctx
val yojson_of_job_ctx : job_ctx -> Yojson.Safe.t
val job_ctx_create : History.item list -> job_ctx
val job_ctx_clone : Id.t -> job_ctx

val dispatch
  :  ?id:Id.t
  -> ?callback:(Instance.t -> unit Lwt.t)
  -> ?message_template:string
  -> ?job_ctx:job_ctx
  -> ?run_at:run_at
  -> Database.Label.t
  -> 'a
  -> 'a Job.t
  -> unit Lwt.t

val dispatch_all
  :  ?callback:(Instance.t -> unit Lwt.t)
  -> ?run_at:run_at
  -> Database.Label.t
  -> (Id.t * 'a * string option * job_ctx) list
  -> 'a Job.t
  -> unit Lwt.t

type kind =
  | Service
  | Worker

val lifecycle_service : Sihl.Container.lifecycle
val lifecycle_worker : Sihl.Container.lifecycle
val hide : ?execute_on_root:bool -> 'a Job.t -> AnyJob.t
val register_jobs : AnyJob.t list -> unit Lwt.t
val register : ?kind:kind -> ?jobs:AnyJob.t list -> unit -> Sihl.Container.Service.t

module Repo : sig
  module Id : sig
    val t : Id.t Caqti_type.t
  end
end
