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

type user =
  { uuid : Pool_common.Id.t
  ; email : string
  }

type t =
  { id : Id.t
  ; changes : Changes.t
  ; model : Pool_message.Field.t
  ; entity_uuid : Pool_common.Id.t
  ; user : user option
  ; created_at : Pool_common.CreatedAt.t
  }

val equal : t -> t -> bool
val show : t -> string
val pp : Format.formatter -> t -> unit

module Write : sig
  type t =
    { id : Id.t
    ; changes : Changes.t
    ; model : Pool_message.Field.t
    ; entity_uuid : Pool_common.Id.t
    ; user_uuid : Pool_common.Id.t option
    ; created_at : Pool_common.CreatedAt.t
    }

  val equal : t -> t -> bool
  val show : t -> string
  val pp : Format.formatter -> t -> unit
end

module DefaultSettings : sig
  val changelog_compare_at_index_keys : string list option
end

module type RecordSig = sig
  type t

  val model : Pool_message.Field.t
  val changelog_compare_at_index_keys : string list option
  val yojson_of_t : t -> Yojson.Safe.t
end

module type TSig = sig
  type record

  val model : Pool_message.Field.t

  val create
    :  ?id:Id.t
    -> ?user_uuid:Pool_common.Id.t
    -> entity_uuid:Pool_common.Id.t
    -> before:record
    -> after:record
    -> unit
    -> Write.t option

  val insert
    :  Database.Label.t
    -> ?user_uuid:Pool_common.Id.t
    -> entity_uuid:Pool_common.Id.t
    -> before:record
    -> after:record
    -> unit
    -> unit Lwt.t
end

module T : functor (R : RecordSig) -> sig
  include TSig with type record = R.t
end

val default_query : Query.t
val filterable_by : Query.Filter.human option
val searchable_by : Query.Column.t list
val sortable_by : Query.Column.t list

val all_by_entity
  :  ?query:Query.t
  -> Database.Label.t
  -> Pool_common.Id.t
  -> (t list * Query.t) Lwt.t
