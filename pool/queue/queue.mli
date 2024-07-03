module Guard = Entity_guard

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
end

module Status : sig
  type t =
    | Pending
    | Succeeded
    | Failed
    | Cancelled

  val min : int
  val max : int
  val to_enum : t -> int
  val of_enum : int -> t option
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
  val ctx : t -> (string * string) list
  val tag : t -> string option
  val last_error_at : t -> Ptime.t option
  val last_error : t -> string option
  val status : t -> Status.t
  val max_tries : t -> int
  val next_run_at : t -> Ptime.t
  val tries : t -> int
  val input : t -> string
  val name : t -> JobName.t
  val id : t -> Id.t
  val is_pending : t -> bool
  val resendable : t -> (t, Pool_message.Error.t) result
  val should_run : t -> Ptime.t -> bool
  val default_error_handler : Database.Label.t -> string -> t -> unit Lwt.t
  val update_next_run_at : Ptime.span -> t -> t
  val increment_tries : Ptime.span -> t -> t

  val create
    :  ?id:Id.t
    -> ?tries:int
    -> ?max_tries:int
    -> ?status:Status.t
    -> ?last_error:string
    -> ?last_error_at:Ptime.t
    -> ?delay:Ptime.span
    -> ?now:Ptime.t
    -> ?tag:string
    -> (string * string) list
    -> JobName.t
    -> string
    -> t
end

module Job : sig
  type 'a t

  val pp : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit
  val show : (Format.formatter -> 'a -> unit) -> 'a t -> string
  val tag : 'a t -> string option
  val retry_delay : 'a t -> Ptime.span
  val max_tries : 'a t -> int
  val failed : 'a t -> Database.Label.t -> string -> Instance.t -> unit Lwt.t

  val handle
    :  'a t
    -> Database.Label.t
    -> Id.t
    -> 'a
    -> (unit, string) Lwt_result.t

  val decode : 'a t -> string -> ('a, string) result
  val encode : 'a t -> 'a -> string
  val name : 'a t -> JobName.t

  val create
    :  ?max_tries:int
    -> ?retry_delay:Ptime.span
    -> ?failed:(Database.Label.t -> string -> Instance.t -> unit Lwt.t)
    -> ?tag:string
    -> (Database.Label.t -> Id.t -> 'a -> (unit, string) Lwt_result.t)
    -> ('a -> string)
    -> (string -> ('a, string) result)
    -> JobName.t
    -> 'a t

  val to_instance
    :  Database.Label.t
    -> 'a
    -> Ptime.span option
    -> Ptime.t
    -> 'a t
    -> Instance.t
end

module AnyJob : sig
  type t

  val pp : Format.formatter -> t -> unit
  val show : t -> string
  val retry_delay : t -> Ptime.span
  val max_tries : t -> int
  val failed : t -> Database.Label.t -> string -> Instance.t -> unit Lwt.t
  val handle : t -> Instance.t -> (unit, string) Lwt_result.t
  val name : t -> JobName.t
end

val hide : 'a Job.t -> AnyJob.t
val column_job_name : Query.Column.t
val column_job_status : Query.Column.t
val column_last_error : Query.Column.t
val column_last_error_at : Query.Column.t
val column_next_run : Query.Column.t
val column_input : Query.Column.t
val job_name_filter : Query.Filter.Condition.Human.t
val job_status_filter : Query.Filter.Condition.Human.t
val searchable_by : Query.Column.t list
val sortable_by : Query.Column.t list
val filterable_by : Query.Filter.Condition.Human.t list option
val default_sort : Query.Sort.t
val default_query : Query.t
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
  :  Database.Label.t
  -> (int, Pool_message.Error.t) Lwt_result.t

val dispatch
  :  ?callback:(Instance.t -> unit Lwt.t)
  -> ?delay:Ptime.span
  -> Database.Label.t
  -> 'a
  -> 'a Job.t
  -> unit Lwt.t

val dispatch_all
  :  ?callback:(Instance.t -> unit Lwt.t)
  -> ?delay:Ptime.span
  -> Database.Label.t
  -> 'a list
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

module History : sig
  type t =
    { entity_uuid : Pool_common.Id.t
    ; job : Instance.t
    ; message_template : string option
    }

  val pp : Format.formatter -> t -> unit
  val show : t -> string
  val message_template : t -> string option
  val job : t -> Instance.t
  val entity_uuid : t -> Pool_common.Id.t

  type create =
    { entity_uuids : Pool_common.Id.t list
    ; message_template : string option
    }

  val pp_create : Format.formatter -> create -> unit
  val show_create : create -> string
  val equal_create : create -> create -> bool
  val create_of_yojson : Ppx_yojson_conv_lib.Yojson.Safe.t -> create
  val yojson_of_create : create -> Ppx_yojson_conv_lib.Yojson.Safe.t

  val create
    :  ?message_template:string
    -> entity_uuid:Pool_common.Id.t
    -> Instance.t
    -> t

  val column_created_at : Query.Column.t
  val filterable_by : Query.Filter.Condition.Human.t list option
  val searchable_by : Query.Column.t list
  val sortable_by : Query.Column.t list
  val default_sort : Query.Sort.t
  val default_query : Query.t

  val create_from_queue_instance
    :  Database.Label.t
    -> create
    -> Instance.t
    -> unit Lwt.t

  val query_by_entity
    :  ?query:Query.t
    -> Database.Label.t
    -> Pool_common.Id.t
    -> (t list * Query.t) Lwt.t

  val find_related
    :  Database.Label.t
    -> Instance.t
    -> [< `contact | `experiment ]
    -> Pool_common.Id.t option Lwt.t
end

module Repo : sig
  module Id : sig
    val t : Id.t Caqti_type.t
  end
end
