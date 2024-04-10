module JobName : sig
  type t =
    | SendEmail
    | SendTextMessage

  val pp : Format.formatter -> t -> unit
  val show : t -> string
  val sexp_of_t : t -> Ppx_sexp_conv_lib.Sexp.t
  val t_of_yojson : Yojson.Safe.t -> t
  val yojson_of_t : t -> Yojson.Safe.t
  val equal : t -> t -> bool
  val read : string -> t
end

module Status : sig
  type t =
    | Pending
    | Succeeded
    | Failed
    | Cancelled

  val pp : Format.formatter -> t -> unit
  val show : t -> string
  val sexp_of_t : t -> Ppx_sexp_conv_lib.Sexp.t
  val t_of_yojson : Yojson.Safe.t -> t
  val yojson_of_t : t -> Yojson.Safe.t
  val equal : t -> t -> bool
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

val pp_instance : Format.formatter -> instance -> unit
val show_instance : instance -> string

type 'a job =
  { name : string
  ; encode : 'a -> string
  ; decode : string -> ('a, string) result
  ; handle : Database.Label.t -> 'a -> (unit, string) result Lwt.t
  ; failed : Database.Label.t -> string -> instance -> unit Lwt.t
  ; max_tries : int
  ; retry_delay : Ptime.span
  ; tag : string option
  }

val pp_job
  :  (Format.formatter -> 'a -> unit)
  -> Format.formatter
  -> 'a job
  -> unit

val show_job : (Format.formatter -> 'a -> unit) -> 'a job -> string

type job' =
  { name : string
  ; handle : Database.Label.t -> string -> (unit, string) result Lwt.t
  ; failed : Database.Label.t -> string -> instance -> unit Lwt.t
  ; max_tries : int
  ; retry_delay : Ptime.span
  }

val pp_job' : Format.formatter -> job' -> unit
val show_job' : job' -> string
val hide : 'a job -> job'
val should_run : instance -> Ptime.t -> bool
val default_tries : int
val default_retry_delay : Ptime.span
val default_error_handler : Database.Label.t -> string -> instance -> unit Lwt.t

val create_job
  :  (Database.Label.t -> 'a -> (unit, string) result Lwt.t)
  -> ?max_tries:int
  -> ?retry_delay:Ptime.span
  -> ?failed:(Database.Label.t -> string -> instance -> unit Lwt.t)
  -> ?tag:string
  -> ('a -> string)
  -> (string -> ('a, string) result)
  -> string
  -> 'a job

val dispatch
  :  ?callback:(instance -> unit Lwt.t)
  -> ?delay:Ptime.span
  -> Database.Label.t
  -> 'a
  -> 'a job
  -> unit Lwt.t

val dispatch_all
  :  ?callback:(instance -> unit Lwt.t)
  -> ?delay:Ptime.span
  -> Database.Label.t
  -> 'a list
  -> 'a job
  -> unit Lwt.t

val lifecycle : Sihl.Container.lifecycle
val register : ?jobs:job' list -> unit -> Sihl.Container.Service.t

val find
  :  Database.Label.t
  -> Pool_common.Id.t
  -> (instance, Pool_message.Error.t) Lwt_result.t

val find_by
  :  ?query:Query.t
  -> Database.Label.t
  -> (instance list * Query.t) Lwt.t

val count_workable
  :  Database.Label.t
  -> (int, Pool_message.Error.t) Lwt_result.t

val is_pending : instance -> bool
val resendable : instance -> (instance, Pool_message.Error.t) Result.t
val column_job_name : Query.Column.t
val column_job_status : Query.Column.t
val column_last_error : Query.Column.t
val column_last_error_at : Query.Column.t
val column_next_run : Query.Column.t
val default_query : Query.t
val filterable_by : Query.Filter.human option
val searchable_by : Query.Column.t list
val sortable_by : Query.Column.t list

module History : sig
  type t

  val pp : Format.formatter -> t -> unit
  val show : t -> string
  val job : t -> instance
  val message_template : t -> string option

  type create =
    { entity_uuids : Pool_common.Id.t list
    ; message_template : string option
    }

  val pp_create : Format.formatter -> create -> unit
  val show_create : create -> string
  val equal_create : create -> create -> bool
  val create_of_yojson : Yojson.Safe.t -> create
  val yojson_of_create : create -> Yojson.Safe.t

  val create
    :  ?message_template:string
    -> entity_uuid:Pool_common.Id.t
    -> instance
    -> t

  val create_from_queue_instance
    :  Database.Label.t
    -> create
    -> instance
    -> unit Lwt.t

  val column_created_at : Query.Column.t
  val default_query : Query.t
  val filterable_by : Query.Filter.human option
  val searchable_by : Query.Column.t list
  val sortable_by : Query.Column.t list

  val query_by_entity
    :  ?query:Query.t
    -> Database.Label.t
    -> Pool_common.Id.t
    -> (t list * Query.t) Lwt.t

  val find_related
    :  Database.Label.t
    -> instance
    -> [< `contact | `experiment ]
    -> Pool_common.Id.t option Lwt.t
end

module Guard : sig
  module Access : sig
    val index : Guard.ValidationSet.t
    val read : Guard.ValidationSet.t
    val resend : Guard.ValidationSet.t
  end
end
