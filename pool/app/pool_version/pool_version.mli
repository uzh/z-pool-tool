module Id : sig
  include module type of Pool_common.Id

  val to_common : t -> Pool_common.Id.t
end

module Version : sig
  include Pool_model.Base.StringSig
end

module Text : sig
  include Pool_model.Base.StringSig
end

module PublishedAt : sig
  include Pool_model.Base.BaseSig

  val value : t -> Ptime.t
  val create : Ptime.t -> t
end

type t =
  { id : Id.t
  ; version : Version.t
  ; text : Text.t
  ; published_at : PublishedAt.t option
  ; created_at : Pool_common.CreatedAt.t
  ; updated_at : Pool_common.UpdatedAt.t
  }

val equal : t -> t -> bool
val pp : Format.formatter -> t -> unit
val show : t -> string
val t_of_yojson : Yojson.Safe.t -> t
val yojson_of_t : t -> Yojson.Safe.t
val create : ?id:Id.t -> Version.t -> Text.t -> t
val find : Id.t -> (t, Pool_message__Pool_message_error.t) Lwt_result.t
val all : ?query:Query.t -> Database.Label.t -> (t list * Query.t) Lwt.t
val column_version : Query.Column.t
val filterable_by : Query.Filter.human option
val searchable_by : Query.Column.t list
val sortable_by : Query.Column.t list
val default_sort : Query.Sort.t
val default_query : Query.t

type event =
  | Created of t
  | Updated of t
  | Published of t

val handle_event : Database.Label.t -> event -> unit Lwt.t
val equal_event : event -> event -> bool
val pp_event : Format.formatter -> event -> unit

module Target : sig
  val to_authorizable
    :  ?ctx:(string * string) list
    -> t
    -> (Guard.Target.t, Pool_message.Error.t) Lwt_result.t

  type t

  val equal : t -> t -> bool
  val pp : Format.formatter -> t -> unit
  val show : t -> string
end

module Access : sig
  val index : Guard.ValidationSet.t
  val create : Guard.ValidationSet.t
  val read : Id.t -> Guard.ValidationSet.t
  val update : Id.t -> Guard.ValidationSet.t
end
