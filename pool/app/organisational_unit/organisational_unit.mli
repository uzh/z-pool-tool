module Id : sig
  include Pool_model.Base.IdSig
end

module Name : sig
  include Pool_model.Base.StringSig
end

type t =
  { id : Id.t
  ; name : Name.t
  }

val equal : t -> t -> bool
val pp : Format.formatter -> t -> unit
val show : t -> string
val t_of_yojson : Yojson.Safe.t -> t
val yojson_of_t : t -> Yojson.Safe.t
val create : ?id:Id.t -> Name.t -> t
val id : t -> Id.t
val name : t -> Name.t

type event =
  | Created of t
  | Updated of (t * Name.t)

val handle_event : Database.Label.t -> event -> unit Lwt.t
val equal_event : event -> event -> bool
val pp_event : Format.formatter -> event -> unit
val show_event : event -> string

module Guard : sig
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
    val update : Id.t -> Guard.ValidationSet.t
    val delete : Id.t -> Guard.ValidationSet.t
  end
end

val find : Database.Label.t -> Id.t -> (t, Pool_message.Error.t) Lwt_result.t
val find_by : Query.t -> Database.Label.t -> (t list * Query.t) Lwt.t
val all : Database.Label.t -> unit -> t list Lwt.t

module Repo : sig
  val sql_select_columns : string list

  module Id : sig
    val t : Id.t Caqti_type.t
  end

  val t : t Caqti_type.t
end

val column_name : Query.Column.t
val default_query : Query.t
val filterable_by : Query.Filter.human option
val searchable_by : Query.Column.t list
val sortable_by : Query.Column.t list

module Changelog : Changelog.TSig with type record = t
