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
  val sihl_queue_to_human : Sihl.Contract.Queue.instance_status -> string
end

val hide : 'a Sihl.Contract.Queue.job -> Sihl.Contract.Queue.job'
val lifecycle : Sihl.Container.lifecycle

val register
  :  ?jobs:Sihl.Contract.Queue.job' list
  -> unit
  -> Sihl.Container.Service.t

val find
  :  Pool_database.Label.t
  -> Pool_common.Id.t
  -> (Sihl_queue.instance, Pool_message.Error.t) Lwt_result.t

val find_by
  :  ?query:Query.t
  -> Pool_database.Label.t
  -> (Sihl_queue.instance list * Query.t) Lwt.t

val count_workable
  :  Pool_database.Label.t
  -> (int, Pool_message.Error.t) Lwt_result.t

val is_pending : Sihl_queue.instance -> bool

val resendable
  :  Sihl_queue.instance
  -> (Sihl_queue.instance, Pool_message.Error.t) Result.t

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
  val job : t -> Sihl.Contract.Queue.instance
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
    -> Sihl_queue.instance
    -> t

  val create_from_queue_instance
    :  Pool_database.Label.t
    -> create
    -> Sihl_queue.instance
    -> unit Lwt.t

  val column_created_at : Query.Column.t
  val default_query : Query.t
  val filterable_by : Query.Filter.human option
  val searchable_by : Query.Column.t list
  val sortable_by : Query.Column.t list

  val query_by_entity
    :  ?query:Query.t
    -> Pool_database.Label.t
    -> Pool_common.Id.t
    -> (t list * Query.t) Lwt.t

  val find_related
    :  Pool_database.Label.t
    -> Sihl_queue.instance
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
