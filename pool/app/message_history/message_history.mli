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

module Repo : sig
  module Entity : sig
    val sihl_queue_job_caqti : Sihl.Contract.Queue.instance Caqti_type.t
  end

  val sql_select_job_queue_columns : string list
end
