module Id : sig
  include Pool_model.Base.IdSig
end

module Change : sig
  type t = Yojson.Safe.t * Yojson.Safe.t

  val equal : t -> t -> bool
  val pp : Format.formatter -> t -> unit
  val show : t -> string
end

module Changes : sig
  type t =
    | Assoc of (string * t) list
    | Change of Change.t
end

type t =
  { id : Id.t
  ; changes : Changes.t
  ; model : Pool_message.Field.t
  ; entity_uuid : Pool_common.Id.t
  ; user_uuid : Pool_common.Id.t
  ; user_email : Pool_user.EmailAddress.t
  ; created_at : Pool_common.CreatedAt.t
  }

module Write : sig
  type t =
    { id : Id.t
    ; changes : Changes.t
    ; model : Pool_message.Field.t
    ; entity_uuid : Pool_common.Id.t
    ; user_uuid : Pool_common.Id.t
    ; created_at : Pool_common.CreatedAt.t
    }
end

module type RecordSig = sig
  type t

  val model : Pool_message.Field.t
  val yojson_of_t : t -> Yojson.Safe.t
end

module type TSig = sig
  type record

  val model : Pool_message.Field.t
  val default_query : Query.t
  val filterable_by : Query.Filter.human option
  val searchable_by : Query.Column.t list
  val sortable_by : Query.Column.t list

  val create
    :  ?id:Id.t
    -> entity_uuid:Pool_common.Id.t
    -> user_uuid:Pool_common.Id.t
    -> before:record
    -> after:record
    -> unit
    -> Write.t option

  val all_by_entity
    :  ?query:Query.t
    -> Database.Label.t
    -> Pool_common.Id.t
    -> (t list * Query.t) Lwt.t
end

module T : functor (R : RecordSig) -> sig
  (* include TSig with type record = R.t *)
  type record = R.t

  val model : Pool_message.Field.t
  val default_query : Query.t
  val filterable_by : Query.Filter.human option
  val searchable_by : Query.Column.t list
  val sortable_by : Query.Column.t list

  val create
    :  ?id:Id.t
    -> entity_uuid:Pool_common.Id.t
    -> user_uuid:Pool_common.Id.t
    -> before:record
    -> after:record
    -> unit
    -> Write.t option

  val all_by_entity
    :  ?query:Query.t
    -> Database.Label.t
    -> Pool_common.Id.t
    -> (t list * Query.t) Lwt.t
end

type event = Created of Entity.Write.t

val equal_event : event -> event -> bool
val pp_event : Format.formatter -> event -> unit
val show_event : event -> string
val handle_event : Database.Label.t -> event -> unit Lwt.t
val created : Write.t -> event
