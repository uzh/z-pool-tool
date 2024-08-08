module Id : sig
  include Pool_model.Base.IdSig
end

type t =
  { id : Id.t
  ; changes : Yojson.Safe.t
  ; model : Pool_message.Field.t
  ; entity_uuid : Pool_common.Id.t
  ; user_uuid : Pool_common.Id.t
  ; created_at : Pool_common.CreatedAt.t
  }

module type RecordSig = sig
  type t

  val model : Pool_message.Field.t
  val yojson_of_t : t -> Yojson.Safe.t
end

module type TSig = sig
  type record

  val model : Pool_message.Field.t

  val create
    :  Database.Label.t
    -> ?id:Id.t
    -> entity_uuid:Pool_common.Id.t
    -> user_uuid:Pool_common.Id.t
    -> before:record
    -> after:record
    -> unit
    -> unit Lwt.t

  val all_by_entity
    :  ?query:Query.t
    -> Database.Label.t
    -> Pool_common.Id.t
    -> (t list * Query.t) Lwt.t
end

module T : functor (R : RecordSig) -> sig
  type record = R.t

  val model : Pool_message.Field.t

  val create
    :  Database.Label.t
    -> ?id:Id.t
    -> entity_uuid:Pool_common.Id.t
    -> user_uuid:Pool_common.Id.t
    -> before:record
    -> after:record
    -> unit
    -> unit Lwt.t

  val all_by_entity
    :  ?query:Query.t
    -> Database.Label.t
    -> Pool_common.Id.t
    -> (t list * Query.t) Lwt.t
end
