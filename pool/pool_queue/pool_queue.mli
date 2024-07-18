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
    -> Database.Label.t
    -> 'a
    -> (unit, Pool_message.Error.t) Lwt_result.t

  val decode : 'a t -> string -> ('a, Pool_message.Error.t) result
  val encode : 'a t -> 'a -> string
  val name : 'a t -> JobName.t

  val create
    :  ?max_tries:int
    -> ?retry_delay:Ptime.span
    -> ?failed:
         (Database.Label.t -> Pool_message.Error.t -> Instance.t -> unit Lwt.t)
    -> (Database.Label.t -> 'a -> (unit, Pool_message.Error.t) Lwt_result.t)
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

  include Repo.ColumnsSig
end

module AnyJob : sig
  type t

  val pp : Format.formatter -> t -> unit
  val show : t -> string
  val retry_delay : t -> Ptime.span
  val max_tries : t -> int

  val failed
    :  t
    -> Database.Label.t
    -> Pool_message.Error.t
    -> Instance.t
    -> unit Lwt.t

  val handle
    :  t
    -> Database.Label.t
    -> string
    -> (unit, Pool_message.Error.t) Lwt_result.t

  val name : t -> JobName.t
end

val hide : 'a Job.t -> AnyJob.t
val register_jobs : AnyJob.t list -> unit Lwt.t

val find
  :  Database.Label.t
  -> Id.t
  -> (Instance.t, Pool_message.Error.t) Lwt_result.t

val find_by
  :  ?query:Query.t
  -> Database.Label.t
  -> (Instance.t list * Query.t) Lwt.t

val count_workable
  :  JobName.t
  -> Database.Label.t
  -> (int, Pool_message.Error.t) result Lwt.t

type mappings =
  | Create of Pool_common.Id.t list
  | Clone of Id.t

val equal_mappings : mappings -> mappings -> bool
val pp_mappings : Format.formatter -> mappings -> unit
val show_mappings : mappings -> string
val mappings_of_yojson : Yojson.Safe.t -> mappings
val yojson_of_mappings : mappings -> Yojson.Safe.t
val mappings_create : Pool_common.Id.t list -> mappings
val mappings_clone : Id.t -> mappings

val dispatch
  :  ?id:Id.t
  -> ?callback:(Instance.t -> unit Lwt.t)
  -> ?message_template:string
  -> ?mappings:mappings
  -> ?run_at:run_at
  -> Database.Label.t
  -> 'a
  -> 'a Job.t
  -> unit Lwt.t

val dispatch_all
  :  ?callback:(Instance.t -> unit Lwt.t)
  -> ?run_at:run_at
  -> Database.Label.t
  -> (Id.t * 'a * string option * mappings) list
  -> 'a Job.t
  -> unit Lwt.t

type kind =
  | Service
  | Worker

val lifecycle_service : Sihl.Container.lifecycle
val lifecycle_worker : Sihl.Container.lifecycle

val register
  :  ?kind:kind
  -> ?jobs:AnyJob.t list
  -> unit
  -> Sihl.Container.Service.t

module Mapping : sig
  type t =
    { entity_uuid : Pool_common.Id.t
    ; job : Instance.t
    }

  val pp : Format.formatter -> t -> unit
  val show : t -> string
  val equal : t -> t -> bool
  val job : t -> Instance.t
  val entity_uuid : t -> Pool_common.Id.t
  val create : Instance.t -> Pool_common.Id.t -> t
  val column_created_at : Query.Column.t

  val query_by_entity
    :  ?query:Query.t
    -> Database.Label.t
    -> Pool_common.Id.t
    -> (t list * Query.t) Lwt.t

  val query_instances_by_entity
    :  ?query:Query.t
    -> Database.Label.t
    -> Pool_common.Id.t
    -> (Entity.Instance.t list * Query.t) Lwt.t

  val find_related
    :  Database.Label.t
    -> Instance.t
    -> [< `contact | `experiment ]
    -> Pool_common.Id.t option Lwt.t

  include Repo.ColumnsSig
end

module JobHistory : Repo.ColumnsSig
