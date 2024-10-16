val url_key : string

module Id : sig
  include module type of Pool_common.Id
end

module Code : sig
  include Pool_model.Base.StringSig
end

module Count : sig
  include Pool_model.Base.IntegerSig
end

type t =
  { id : Id.t
  ; code : Code.t
  ; count : Count.t
  }

type event = Updated of Code.t

val equal_event : event -> event -> bool
val pp_event : Format.formatter -> event -> unit
val show_event : event -> string
val handle_event : Database.Label.t -> event -> unit Lwt.t
val insert : Database.Label.t -> string -> unit Lwt.t
val all : ?query:Query.t -> Database.Label.t -> (t list * Query.t) Lwt.t
val column_code : Query.Column.t
val column_count : Query.Column.t
val searchable_by : Query.Column.t list
val sortable_by : Query.Column.t list
val filterable_by : Query.Filter.human option
val default_sort : Query.Sort.t
val default_query : Query.t

module Access : sig
  val index : Guard.ValidationSet.t
end
