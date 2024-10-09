module Id : sig
  include Pool_model.Base.IdSig
end

module Name : sig
  include Pool_model.Base.StringSig
end

module Token : sig
  type t

  val generate : unit -> t
  val value : t -> string
end

type t =
  { id : Id.t
  ; name : Name.t
  ; token : Token.t
  ; created_at : Pool_common.CreatedAt.t
  ; updated_at : Pool_common.UpdatedAt.t
  }

val equal : t -> t -> bool
val pp : Format.formatter -> t -> unit
val show : t -> string
val sexp_of_t : t -> Sexplib0.Sexp.t
val t_of_yojson : Yojson.Safe.t -> t
val yojson_of_t : t -> Yojson.Safe.t
val create : ?id:Id.t -> ?token:Token.t -> Name.t -> t

val find
  :  Database.Label.t
  -> Id.t
  -> (t, Pool_message__Pool_message_error.t) result Lwt.t

val find_by_token : Database.Label.t -> string -> t option Lwt.t
val all : ?query:Query.t -> Database.Label.t -> (t list * Query.t) Lwt.t

type event =
  | Created of t
  | Updated of (t * t)
  | Destroyed of t

val handle_event : ?tags:Logs.Tag.set -> Database.Label.t -> event -> unit Lwt.t
val equal_event : event -> event -> bool
val pp_event : Format.formatter -> event -> unit
val filterable_by : 'a option
val column_name : Query.Column.t
val column_created_at : Query.Column.t
val searchable_by : 'a list
val sortable_by : Query.Column.t list
val default_sort : Query.Sort.t
val default_query : Query.t

module Access : sig
  val index : Guard.ValidationSet.t
  val create : Guard.ValidationSet.t
  val read : Id.t -> Guard.ValidationSet.t
  val update : Id.t -> Guard.ValidationSet.t
end

module Actor : sig
  val to_authorizable
    :  ?ctx:(string * string) list
    -> t
    -> (Guard.Actor.t, Pool_message.Error.t) Lwt_result.t

  type t

  val equal : t -> t -> bool
  val pp : Format.formatter -> t -> unit
  val show : t -> string
end
